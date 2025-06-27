#' Calculate intercept and slope of linear transform
#'
#' This function obtains the intercept and slope of a given
#' linear transformation.
#' @param original Numeric vector of original values
#' @param transformed Numeric vector of transformed values
#' @return A vector of length two with the intercept and slope
#' @noRd
calculate_transform <- function(original, transformed) {
  coef(lm(transformed ~ original))
}
