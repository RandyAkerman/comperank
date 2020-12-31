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
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  fit <- massey2_impl(predictors, outcome)

  new_massey2(
    coefs = fit$coefs,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

massey2_impl <- function(predictors, outcome) {
  list(coefs = 1)
}
