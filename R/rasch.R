#' Pairwise Estimation of the Rasch Model under Item Equating
#'
#' This function uses pairwise conditional likelihood estimation for
#' estimating item parameters in the Rasch model.
#' @aliases rasch
#' @param data A matrix or a data frame with item responses. The data can take
#' two shapes. In the **wide shape** the data is a matrix or data frame
#' with items in columns and visits in the rows. In the **long shape** the data
#' is a data frame with item responses in long format, i.e. with
#' visit identifying variables, a column with item names and a column
#' with responses. The wide format is simpler to work with, but the long
#' format allows for more flexibility in the data structure and is more
#' efficient for large datasets. The responses should be coded as `1` (pass) or
#' `0` (fail). Missing responses are allowed and are coded as \code{NA}.
#' @param shape Character string indicating the shape of the data, either
#' `"wide"`, `"long"` or `"auto"`. If not specified, the function tries
#' to determine the shape automatically.
#' @param visit_var *Only relevant for long shape*. Character vector with
#' names of the columns identifying unique visits. Ability is assumed to be
#' constant for all measurements made during the visit. The default names
#' are `visit_var = c("subjid", "agedays")`.
#' @param item_var *Only relevant for long shape*. Character string with the
#' name of the column containing item names. The default is `item_var = "item"`.
#' @param response_var *Only relevant for long shape*. Character string with
#' the name of the column containing responses. The default is
#' `response_var = "response"`.
#' @param items Character vector with item names. If not specified,
#' all columns in the data are included as items (for wide shape) or
#' all items in the item column are included (for long shape).
#' @param equate A named list of active equates. Each list element specifies
#' a vector of item names belonging to the same equate group. The name
#' of the list element is the equate group name.
#' The method restricts the difficulty estimates of items within an
#' active equate to be identical. Note that a given item should appear
#' only once in an equate group. The default `equate = NULL` does
#' not to restrict the solution.
#' @param b_fixed Numeric, named vector used for fixing the item parameter
#' estimate to a specific value. The names of the vector indicate
#' the item name to which the fixed value applies.
#' @param b_init Numeric, named vector of initial item difficulty estimates.
#' Under the default (\code{NULL}) values initial values are calculated
#' internally.
#' @param pairs A table of counts \code{t(data == 0)} times \code{data == 1}
#' for all items specified by `items`.
#' The default (\code{NULL}) calculates the pairs table for the active item
#' set in the data. This step can take substantial execution time
#' in large dataset. In that case, it is recommended to pre-compute the
#' pairs table and pass it to this function.
#' @param conv Convergence criterion in maximal absolute parameter change
#' @param maxiter Maximal number of iterations
#' @param progress A logical which displays the iterative process.
#' Default is \code{FALSE}.
#' @param zerosum Optional logical indicating whether item difficulties
#' should be centered in each iteration. The default is that no centering
#' is conducted.
#' @param save_pairs Logical. Save the pairs object in the result?
#' @param save_wide Logical. Save the data (wide format) in the result?
#'
#' @details
#'
#' This function is loosely based on `sirt::rasch.pairwise.itemcluster()`.
#'
#' The `rasch_wide()` and `rasch_long()` functions provide a few special
#' capabilities:
#'
#' 1. They allow the user to specify a set of items that should be equated.
#' 2. They allow the user to fix item parameters to a specific value.
#' 3. They allow the user to specify a pairs table, which can speed up
#'    subsequent calls for the same set of items.
#' @author Stef van Buuren
#' @return A list with the following elements:
#'
#' - `item`: A character vector containing the names of the items for which
#'  item parameters were estimated.
#' - `visit_var`: A character vector with the names of the variables to define
#'  unique visits.
#' - `item_var`: The name of the variable containing item names.
#' - `response_var`: The name of the variable containing item responses.
#' - `ability_var`: The name of the ability variable (e.g, `"dscore"`)
#' - `shape`: The shape of the data, either `"wide"` or `"long"`.
#' - `b_fixed`: The `b_fixed` argument values.
#' - `equate`: The `equate` argument values.
#' - `b_init`: The `b_init` argument values.
#' - `orphans`: A character vector with item names that are not connected
#' to any other item in the data.
#' - `zerosum`: The `zerosum` argument value.
#' - `iter`: Iteration counter.
#' - `convergence`: Convergence criterion.
#' - `item`: Data frame with estimated item parameters (`n`, `p`, `b`).
#' - `betapar`: A named vector with negated item difficulties.
#' - `call`: The matched call to the function.
#' - `pairs`: The pairs table used by the algorithm, if `save_pairs` is `TRUE`.
#'   May be used to speed up subsequent calls for the same set of items.
#' - `wide`: The data in wide format, if `save_wide` is `TRUE`.
#'
#' @references van der Linden, W. J., & Eggen, T. J. H. M. (1986).
#' \emph{An empirical Bayes approach to item banking}. Research Report 86-6,
#' University of Twente.
#'
#' Zwinderman, A. H. (1995). Pairwise parameter estimation in Rasch models.
#' \emph{Applied Psychological Measurement}, \bold{19}, 369-375.
#'
#' @note
#' No standard errors are provided by this function. Use resampling methods
#' for conducting statistical inference. Formulas for asymptotic standard
#' errors of this pairwise estimation method are described in Zwinderman (1995).
#' @rdname rasch
#' @export
rasch <- function(data,
                  shape = c("auto", "wide", "long"),
                  visit_var = c("subjid", "agedays"),
                  item_var = "item",
                  response_var = "response",
                  items = NULL,
                  equate = NULL,
                  b_fixed = NULL,
                  b_init = NULL,
                  zerosum = FALSE,
                  pairs = NULL,
                  conv = .00001,
                  maxiter = 3000,
                  progress = FALSE,
                  save_pairs = FALSE,
                  save_wide = FALSE) {
  call <- match.call()
  shape <- match.arg(shape)

  if (shape == "auto") {
    if (is.data.frame(data) &&
        all(c(visit_var, item_var, response_var) %in% colnames(data))) {
      shape <- "long"
    } else if (is.matrix(data) || is.data.frame(data)) {
      shape <- "wide"
    } else {
      stop("Data must be a matrix or data frame.")
    }
  }

  if (shape == "wide")
    results <- rasch_wide(wide = data,
                          visit_var = visit_var,
                          items = items,
                          equate = equate,
                          b_fixed = b_fixed,
                          b_init = b_init,
                          pairs = pairs,
                          zerosum = zerosum,
                          conv = conv,
                          maxiter = maxiter,
                          progress = progress,
                          save_wide = save_wide)
  else if (shape == "long")
    results <- rasch_long(long = data,
                          visit_var = visit_var,
                          item_var = item_var,
                          response_var = response_var,
                          items = items,
                          equate = equate,
                          b_fixed = b_fixed,
                          b_init = b_init,
                          pairs = pairs,
                          zerosum = zerosum,
                          conv = conv,
                          maxiter = maxiter,
                          progress = progress,
                          save_wide = save_wide)
  else
    stop("Unknown shape: ", shape)

  results$call <- call
  return(results)
}
