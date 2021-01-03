#' Fit a `massey2`
#'
#' `massey2()` fits a model.
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
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#'
#' # XY interface
#' mod <- massey2(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- massey2(mpg ~ ., mtcars)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(mpg ~ ., mtcars)
#' rec <- step_log(rec, disp)
#' mod3 <- massey2(rec, mtcars)
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
  processed <- hardhat::mold(formula, data)
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
  # TODO: Change verbiage of predictors to matches
  # TODO: Remove outcomes because it doesn't apply for the massey model
  predictors <- processed$predictors
  # outcome <- processed$outcomes[[1]]

  # TODO: Write data conversion code here
  fit <- massey2_impl(predictors)

  new_massey2(
    # TODO: Include what we need to make predictions on future game
    ratings = fit$ratings,
    rankings = fit$rankings,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

massey2_impl <- function(predictors, type = "desc",
                         ties = c("average", "first", "last",
                                  "random", "max", "min"),
                         round_digits = 7) {
  # TODO: Move the data conversion code to the bridge function above
  # Data conversion ------------------
  cr <- as_longcr(predictors, repair = TRUE)

  assert_pairgames(cr)

  # Assert used players
  players <- levels2(cr$player)
  original_players <- unique(cr$player)
  assert_used_objects(used = players, original = original_players,
                      prefix = "rate_massey: ", object_name = "players",
                      data_name = "competition results")
  # ----------------------------------------------------
  # Compute Massey ratings
  h2h_mat <- h2h_mat(cr, !!h2h_funs[["num"]], fill = 0)
  games_played_mat <- h2h_mat
  # TODO: Add this to comepres::h2h_funs as games_played
  games_played_mat[upper.tri(games_played_mat) | lower.tri(games_played_mat)] <- 0
  massey_mat <- -h2h_mat
  diag(massey_mat) <- 0
  diag(massey_mat) <- - rowSums(massey_mat)

  sum_score_mat <- h2h_mat(cr, !!h2h_funs[["sum_score"]], fill = 0)
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

  overall_ratings <- enframe_vec(res_vec, unique_levels(cr$player), "player", "rating_overall")
  defensive_rating <- enframe_vec(def_vec, unique_levels(cr$player), "player", "rating_defensive")
  offensive_rating <- enframe_vec(off_vec, unique_levels(cr$player), "player", "rating_offensive")

  ratings <-
    tibble::tibble(player = original_players) %>%
    dplyr::left_join(overall_ratings, by = "player") %>%
    dplyr::left_join(defensive_rating, by = "player") %>%
    dplyr::left_join(offensive_rating, by = "player")

  # TODO: Implement the offensive and defensive ranking.  Look into dplyr::across
  rank_vec <- comperank::round_rank(
    res_vec, type = type,
    ties = ties, round_digits = round_digits)

  rankings <- enframe_vec(rank_vec, unique_levels(cr$player), "player", "ranking_massey")

  # return a list of rankings and ratings
  # TODO: Decide if we want to return the named vector or the tibble
  list(rankings = rankings, ratings = ratings)
}
