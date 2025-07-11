#' Plot passing probability per item, by age or D-score
#'
#' This function produces a canned \code{ggplot} object.
#' @inheritParams rasch
#' @param pass A `data.frame` with empirical probabilities per item.
#' For age, the expected columns are `item`, `cohort`, `p`, `a`, `n`,
#' `equate` and `label`.
#' For D-score, the expected columns are `item`, `cohort`, `dcut`, `p`, `d`,
#' `n`, `equate` and `label`.
#' @param data_rug Optional data frame of individual responses to the item,
#'   used to add rug plots. Should include `item`, `age`/`dscore`,
#'   `response` and `cohort`.
#' @param x_var Variable representing the continuous x-axis
#' (e.g., age in months, or D-score).
#' @param by_var Variable by which to group the data for plotting (e.g.
#' cohort).
#' @param model_name Optional model name used as annotation in the plot.
#' @param xlim Numeric vector of length 2 specifying x-axis limits
#'   (in months). Defaults to `c(0, 60)`.
#' @param xbreaks Sequence of breaks for the x-axis. Defaults to
#'   `seq(0, 60, 6)`.
#' @param xlab Label for the x-axis. Defaults to `"Age (in months)"`.
#' @param label_trunc Maximum length of the item label to display.
#' @param col_manual Optional named vector of manual colors by `cohort`.
#'   If `NULL`, colors are obtained using `get_palette()`.
#' @param min_n Minimum number of observations required to include a point.
#'   Defaults to 10.
#' @param na_value Color to use for missing values. Default is `"grey"`.
#' @return A `ggplot` object.
#' @export
plot_pass <- function(pass,
                      data_rug = NULL,
                      items = NULL,
                      item_var = "item",
                      response_var = "response",
                      x_var = "a",
                      by_var = "cohort",
                      min_n = 10,
                      model_name = "",
                      xlim = NULL,
                      xbreaks = NULL,
                      xlab = NULL,
                      label_trunc = 60,
                      col_manual = NULL,
                      na_value = "grey") {

  # pre-allocate list of ggplots
  if (is.null(items)) {
    items <- stri_sort(unique(pass[[item_var]]), numeric = TRUE)
  }

  # Determine whether we should plat against age or D-score
  if ("a" %in% colnames(pass)) {
    x_var <- "a"
  } else if ("d" %in% colnames(pass)) {
    x_var <- "d"
  } else {
    stop("Neither 'a' nor 'd' found in pass data.")
  }

  if (x_var == "a") {
    xlim <- c(0, 60)
    xbreaks <- seq(0, 60, 6)
    xlab <- "Age (in months)"
  } else {
    xlim <- c(0, 100)
    xbreaks <- seq(0, 100, 10)
    xlab <- "Ability (D-score)"
  }

  plot_list <- vector("list", length(items))
  names(plot_list) <- items

  # loop over plots
  for (i in seq_along(plot_list)) {
    cat("Item: ", as.character(i), items[i], "\n")
    plot_list[[i]] <- plot_pass_item(pass = pass,
                                     data_rug = data_rug,
                                     by_value = items[i],
                                     i = i,
                                     item_var = item_var,
                                     response_var = response_var,
                                     x_var = x_var,
                                     by_var = by_var,
                                     min_n = min_n,
                                     model_name = model_name,
                                     xlim = xlim,
                                     xbreaks = xbreaks,
                                     xlab = xlab,
                                     label_trunc = label_trunc,
                                     col_manual = col_manual,
                                     na_value = na_value)
  }

  return(plot_list)
}
