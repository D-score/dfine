#' Plot probability by age, all items
#'
#' This function produces a canned \code{ggplot} object.
#' @inheritParams rasch
#' @param pass A `data.frame` with emperical probabilities per item per month,
#' with expected columns `item`, `cohort`, `agegp`, `p`, `a`, `n`, `equate`
#' and `label`.
#' @param model_name A string with the model name, used in the plot title.
#' @param \dots Additional parameters passed down to \code{plot_p_a_one_item()}.
#' @return List of \code{ggplot} objects
#' @export
plot_p_a_item <- function(pass,
                          item_var = "item",
                          items = NULL,
                          model_name = "",
                          ...) {
  # pre-allocate list of ggplots
  if (is.null(items)) {
    items <- stri_sort(unique(pass[[item_var]]), numeric = TRUE)
  }
  plot_list <- vector("list", length(items))
  names(plot_list) <- items

  # loop over plots
  for (i in seq_along(plot_list)) {
    cat("Item: ", as.character(i), items[i], "\n")
    plot_list[[i]] <- plot_p_a_one_item(pass,
                                        item_var = item_var,
                                        by_value = items[i],
                                        i = i,
                                        model_name = model_name,
                                        ...)
  }

  return(plot_list)
}
