#' Calculate the local itemtable from a given Rasch model
#'
#' This function calculates the itemtable from a fitted Rasch model that
#' specifies equate groups different from the default \code{ddata::itemtable}.
#' This resulting local itemtable can be used to calculate fit statistics
#' and plot the results from Rasch models with custimomized equates.
#' @param items  A vector of item names
#' @param equatelist A list with equate groups
#' @param itemtable An itemtable against which equates should be checked.
#' @param activenames A vector of names of active equate groups. Active
#' equates are signalled in the column \code{active}.
#' @return A data frame with four columns and number of rows equal to
#' \code{length(items)}.
#' @noRd
calculate_itemtable <- function(items = NULL,
                                equatelist = NULL,
                                itemtable = NULL,
                                activenames = NULL) {

  # subset items fetch itemtable, dynamic ("") or built-in (dscore, ddata)
  it <- dscore::get_itemtable(items = items, itemtable = itemtable)

  # no equates, then we're done
  if (is.null(equatelist)) {
    it$active_equate <- FALSE
    return(it)
  }

  # validate equatelist
  if (!is.list(equatelist))
    stop("'equatelist' not a list")
  equatenames <- names(equatelist)
  if (any(duplicated(equatenames)))
    stop("'equatelist' has duplicate list names: ",
         equatenames[duplicated(equatenames)])
  items_eq <- unlist(equatelist)
  if (any(duplicated(items)))
    stop("'equatelist' has duplicate items: ",
         items_eq[duplicated(items_eq)])

  # update to account for equate groups changed by user
  for (eq in equatenames) {
    # destroy stored equate
    idx <- !is.na(it$equate) & it$equate == eq
    it[idx, "equate"] <- NA_character_
    # overwrite equate field of items belonging to user equate
    idy <- it$item %in% equatelist[[eq]]
    it[idy, "equate"] <- eq
  }

  # add active column
  it$active_equate <- FALSE
  it[it$equate %in% activenames, "active_equate"] <- TRUE

  it[dscore::order_itemnames(it$item), ]
}
