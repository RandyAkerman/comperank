#' Massey method
#'
#' `massey2()` fits a model using the Massey Method
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `massey2` object.
#'
#' @examples
#' predictors <-
#'   ncaa2005 %>%
#'   dplyr::select(game, player)
#'
#' outcome <-
#'   ncaa2005 %>%
#'   dplyr::select(score)
#'
#' # XY interface
#' mod <- massey2(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- massey2(score ~ ., ncaa2005)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(score ~ ., ncaa2005)
#' mod3 <- massey2(rec, ncaa2005)
#'
#' @export
massey2 <- function(x, ...) {
  UseMethod("massey2")
}

#' @export
#' @rdname massey2
massey2.default <- function(x, ...) {
  stop("`massey2()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname massey2
massey2.data.frame <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  massey2_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname massey2
massey2.matrix <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  massey2_bridge(processed, ...)
}

# Formula method

#' @export
#' @rdname massey2
massey2.formula <- function(formula, data, ...) {
  processed <- hardhat::mold(formula, data,
                             blueprint = hardhat::default_formula_blueprint(indicators = "none"))
  massey2_bridge(processed, ...)
}

# Recipe method

#' @export
#' @rdname massey2
massey2.recipe <- function(x, data, ...) {
  processed <- hardhat::mold(x, data)
  massey2_bridge(processed, ...)
}

# ------------------------------------------------------------------------------
# Bridge

massey2_bridge <- function(processed, ...) {
  # TODO: Clean this up, it feels like I am jumping through hoops to get the XY blueprint working

  # NOTE: The filtering of NA in the player column is a deviation from the
  # upstream comperank massey function.  I think it makes sense to do so because
  # otherwise the massey matrix diagonals reflect games played against unknown
  # players.  It doesn't seem to make a difference in the overall rating but it
  # does make a difference in the offensive and defensive ratings.

  # TODO: Write warning about filtering games with NA players
  # TODO: Write warning about converting players as factors

  cr_data <-
    cbind(processed$predictors, processed$outcomes) %>%
    dplyr::group_by(game) %>%
    comperes::as_longcr()

  if (is.factor(cr_data$player)==FALSE) {
    message('player datatype will be converted to a factor')
    cr_data <-
      cr_data %>%
      dplyr::mutate(
        player = forcats::as_factor(player),
        player = forcats::fct_explicit_na(player))
  }

  if (anyNA(cr_data$player)==TRUE) {
    message('Matches with player `NA` will be removed')
    cr_data <-
      cr_data %>%
      dplyr::group_by(game) %>%
      dplyr::filter(!(NA %in% player))
  }

  if (length(setdiff(levels(cr_data$player),cr_data$player)) != 0) {
    # TODO: Improve this message by lising which players are missing
    message('Dropping levels in the player factor that do not appear in games')
    cr_data <-
      cr_data %>%
      dplyr::mutate(player = forcats::fct_drop(player))
  }

  # cr_data <-
  #   cbind(processed$predictors, processed$outcomes) %>%
  #   dplyr::group_by(game) %>%
  #   dplyr::filter(!(NA %in% player)) %>%
  #   dplyr::mutate(player = forcats::as_factor(player),
  #                 player = forcats::fct_drop(player)) %>%
  #   comperes::as_longcr()

  assert_pairgames(cr_data)

  # Assert used players
  # players <- levels2(cr_data$player)
  # original_players <- unique(cr_data$player)
  # assert_used_objects(used = players, original = original_players,
  #                     prefix = "rate_massey: ", object_name = "players",
  #                     data_name = "competition results")

  fit <- massey2_impl(cr_data, players, original_players)

  new_massey2(
    ratings = fit$ratings,
    rankings = fit$rankings,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

massey2_impl <- function(cr_data, players, original_players, type = "desc",
                         ties = c("average", "first", "last",
                                  "random", "max", "min"),
                         round_digits = 7) {

  # Compute Massey ratings
  h2h_mat <- h2h_mat(cr_data, !!h2h_funs[["num"]], fill = 0)
  games_played_mat <- h2h_mat
  # TODO: Add this to comepres::h2h_funs as games_played
  games_played_mat[upper.tri(games_played_mat) | lower.tri(games_played_mat)] <- 0
  massey_mat <- -h2h_mat
  diag(massey_mat) <- 0
  diag(massey_mat) <- - rowSums(massey_mat)

  sum_score_mat <- h2h_mat(cr_data, !!h2h_funs[["sum_score"]], fill = 0)
  diag(sum_score_mat) <- 0

  score_for <- rowSums(sum_score_mat)
  score_against <- colSums(sum_score_mat)
  score_diff <- score_for - score_against

  massey_mat_mod <- massey_mat
  massey_mat_mod[nrow(massey_mat_mod), ] <- 1
  score_diff_mod <- score_diff
  score_diff_mod[length(score_diff_mod)] <- 0

  res_vec <- solve(massey_mat_mod, score_diff_mod)

  def_vec <- solve(h2h_mat, as.numeric(games_played_mat %*% res_vec) - rowSums(sum_score_mat))

  off_vec <- res_vec - def_vec

  # TODO: There has got to be a tidy-er way to do this
  overall_ratings <- enframe_vec(res_vec, unique_levels(cr_data$player),
                                 "player", "rating_overall")
  defensive_rating <- enframe_vec(def_vec, unique_levels(cr_data$player),
                                  "player", "rating_defensive")
  offensive_rating <- enframe_vec(off_vec, unique_levels(cr_data$player),
                                  "player", "rating_offensive")

  ratings <-
    tibble::tibble(player = unique_levels(cr_data$player)) %>%
    dplyr::left_join(overall_ratings, by = "player") %>%
    dplyr::left_join(defensive_rating, by = "player") %>%
    dplyr::left_join(offensive_rating, by = "player")

  overall_rank_vec <- comperank::round_rank(res_vec, type = type, ties = ties,
                                            round_digits = round_digits)
  defensive_rank_vec <- comperank::round_rank(def_vec, type = type, ties = ties,
                                            round_digits = round_digits)
  offensive_rank_vec <- comperank::round_rank(off_vec, type = type, ties = ties,
                                            round_digits = round_digits)

  overall_rankings <- enframe_vec(overall_rank_vec, unique_levels(cr_data$player),
                                  "player", "ranking_overall")
  defensive_rankings <- enframe_vec(defensive_rank_vec, unique_levels(cr_data$player),
                                  "player", "ranking_defensive")
  offensive_rankings <- enframe_vec(offensive_rank_vec, unique_levels(cr_data$player),
                                  "player", "ranking_offensive")

  rankings <-
    tibble::tibble(player = unique_levels(cr_data$player)) %>%
    dplyr::left_join(overall_rankings, by = "player") %>%
    dplyr::left_join(defensive_rankings, by = "player") %>%
    dplyr::left_join(offensive_rankings, by = "player")

  # return a list of rankings and ratings
  list(rankings = rankings, ratings = ratings)
}
