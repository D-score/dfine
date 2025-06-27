#' Calculate the itembank for a given set of items
#'
#' @param key The model from which the itembank should be calculated.
#' @param tau A named vector with item difficulties.
#' @param itemtable An itemtable with descriptive information per item. The
#' default, \code{itemtable = dscore::get_itemtable()}, takes the built-in item table
#' from the \code{dscore} package.
#' @param item_var The name of the item variable.
#' @return A data frame with at least columns named \code{"key"},
#' \code{"item"} and \code{"tau"}.
#' @noRd
calculate_itembank <- function(key,
                               tau,
                               itemtable = dscore::get_itemtable(),
                               item_var = NULL) {

  if (is.null(item_var)) {
    item_var <- "item"
  } else {
    item_var <- as.character(item_var[1L])
  }
  # create local itembank for D-score calculation
  tau_df <- data.frame(key = key,
                       item = names(tau),
                       tau = tau)
  itembank <- left_join(itemtable, tau_df, by = item_var) |>
    filter(!is.na(tau))
  itembank[stringi::stri_order(itembank$item, numeric = TRUE), ]
}
