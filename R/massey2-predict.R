#' Predict from a `massey2`
#'
#' @param object A `massey2` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' train <- mtcars[1:20,]
#' test <- mtcars[21:32, -1]
#'
#' # Fit
#' mod <- massey2(mpg ~ cyl + log(drat), train)
#'
#' # Predict, with preprocessing
#' predict(mod, test)
#'
#' @export
predict.massey2 <- function(object, new_data, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_massey2_predict_types())
  predict_massey2_bridge(type, object, forged$predictors)
}

valid_massey2_predict_types <- function() {
  c("numeric", "class")
}

# ------------------------------------------------------------------------------
# Bridge

predict_massey2_bridge <- function(type, model, predictors) {
  # TODO: Write two tracks one where predictors are null i.e no shcedule of upcoming games is given or if an upcoming game is given for new _data
  # predictors <- as.matrix(predictors)
  predict_function <- get_massey2_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

# TODO: Decide if the terminology in the type of predictions we want should be gambling or data science
get_massey2_predict_function <- function(type) {
  switch(
    type,
    class = predict_massey2_class,
    numeric = predict_massey2_numeric
  )
}

# ------------------------------------------------------------------------------
# Implementation

predict_massey2_class <- function(model, predictors) {
  # TODO: Clean this up a bit.  It feels clunky
  # TODO: validate that this works on game with more than 2 players
  matchups <-
    predictors %>%
    dplyr::left_join(model$rankings, by = "player") %>%
    dplyr::group_by(.data$game) %>%
    dplyr::mutate(pred_class = dplyr::if_else(.data$ranking_massey == min(.data$ranking_massey),
                                         "winner", "loser")) %>%
    dplyr::ungroup()

  predictions <- as.factor(matchups$pred_class)

  hardhat::spruce_class(predictions)
}

predict_massey2_numeric <- function(model, predictors) {
  # TODO: Figure out how this scales to games with more than 2 players or warn a user from doing so
  matchups <-
    predictors %>%
    dplyr::left_join(model$ratings, by = "player") %>%
    dplyr::group_by(.data$game) %>%
    dplyr::mutate(pred = .data$rating_offensive - sum(.data$rating_defensive) + .data$rating_defensive) %>%
    dplyr::ungroup()

  predictions <- as.numeric(matchups$pred)

  hardhat::spruce_numeric(predictions)
}
