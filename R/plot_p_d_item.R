#' Plot probability by D-score, all items
#'
#' This function produces a canned \code{ggplot} object.
#' @inheritParams rasch
#' @param pass A `data.frame` with emperical probabilities per item per month,
#' with expected columns `item`, `cohort`, `agegp`, `p`, `a`, `n`, `equate`
#' and `label`.
#' @param x_var Variable representing the continuous x-axis
#' (e.g., age in months, or D-score).
#' @param model_name A string with the model name, used in the plot title.
#' @param xlim Numeric vector of length 2 specifying x-axis limits
#'   (in months). Defaults to `c(0, 80)`.
#' @param xbreaks Sequence of breaks for the x-axis.
#' @param \dots Additional parameters passed down to \code{plot_p_a_one_item()}.
#' @return List of \code{ggplot} objects
#' @export
plot_p_d_item <- function(pass,
                          item_var = "item",
                          items = NULL,
                          x_var = "d",
                          model_name = "",
                          xlim = c(0, 100),
                          xbreaks = seq(xlim[1], xlim[2], diff(xlim)/10),
                          ...) {

  # pre-allocate list of ggplots
  if (is.null(items)) {
    items <- stri_sort(unique(pass[[item_var]]), numeric = TRUE)
  }
  plot_list <- vector("list", length(items))
  names(plot_list) <- items

  x.label <- paste("Ability (D-score) ", model_name)

  # loop over plots
  for (i in seq_along(plot_list)) {
    cat("Item: ", as.character(i), items[i], "\n")
    plot_list[[i]] <- plot_p_a_one_item(pass,
                                        item_var = item_var,
                                        by_value = items[i],
                                        x_var = x_var,
                                        xlab = x.label,
                                        i = i,
                                        xlim = xlim,
                                        xbreaks = xbreaks,
                                        ...)
  }

  return(plot_list)
}
