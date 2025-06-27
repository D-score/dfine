#' Bind active and passive equates
#'
#' Bind the active and background equates into a single equate list.
#' Item names not appearing in \code{items} are removed. The function
#' removes any equates containing less that two items.
#' @inheritParams fit_dmodel
#' @param items Character vector of items in the model. If not
#' specified, the function checks against the vector of item names
#' produced by \code{dscore::get_itemnames()}.
#' @return A list of equates, possibly of length zero
#' @noRd
bind_equates <- function(equate,
                         background_equate = FALSE,
                         items = dscore::get_itemnames()) {

  # active equates from model
  list1 <- equate
  list2 <- background_equate

  # if needed, append equates from itemtable
  if (is.logical(background_equate)) {
    list2 <- NULL
    if (background_equate) {
      from <- NULL
      it <- dscore::get_itemtable(items = items,
                                  itemtable = from)
      list2 <- split(it$item, it$equate)
    }
  }

  # combine lists, remove duplicate equates from equatelist2
  equatelist <- c(list1, list2)
  equatelist <- equatelist[!duplicated(names(equatelist))]

  # remove items not present from equates
  items_eq <- unique(unlist(equatelist))
  items_eq <- intersect(items, items_eq)
  equatelist <- lapply(equatelist, function(x) x[x %in% items_eq])

  # remove equates with fewer than 2 items
  n_items <- sapply(equatelist, length)
  equatelist <- equatelist[n_items > 1]

  # order
  equatelist[stringi::stri_order(names(equatelist), numeric = TRUE)]
}
