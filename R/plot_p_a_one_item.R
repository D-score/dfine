#' Plot pass rates by a continuous x for a single item
#'
#' Creates a line plot of the percentage of children passing a specific item
#' as a function of age, D-score, or other continuous item,
#' optionally including individual-level responses as rug plots.
#'
#' @inheritParams rasch
#' @param pass A data frame containing summarized pass rate data, with
#'   variables including `item`, `p` (percent pass), `a` (mean age),
#'   `n` (sample size), and `cohort`.
#' @param data_rug Optional data frame of individual responses to the item,
#'   used to add rug plots. Should include `item`, `age`/`dscore`,
#'   `response` and `cohort`.
#' @param x_var Variable representing the continuous x-axis
#' (e.g., age in months, or D-score).
#' @param by_var Variable by which to group the data for plotting.
#' @param by_value Value of the item to filter and plot.
#' @param i Item index or number (used for annotation).
#' @param min_n Minimum number of observations required to include a point.
#'   Defaults to 10.
#' @param model_name Optional model name used as annotation in the plot.
#' @param xlim Numeric vector of length 2 specifying x-axis limits
#'   (in months). Defaults to `c(0, 60)`.
#' @param xbreaks Sequence of breaks for the x-axis. Defaults to
#'   `seq(0, 60, 6)`.
#' @param xlab Label for the x-axis. Defaults to `"Age (in months)"`.
#' @param label.trunc Maximum length of the item label to display.
#' @param col.manual Optional named vector of manual colors by `cohort`.
#'   If `NULL`, colors are obtained using `get_palette()`.
#' @param na.value Color to use for missing values. Default is `"grey"`.
#'
#' @return A `ggplot` object.
#' @export
plot_p_a_one_item <- function(pass,
                              data_rug = NULL,
                              item_var = "item",
                              response_var = "response",
                              x_var = "agemos",
                              by_var = "cohort",
                              by_value,
                              i = 0,
                              min_n = 10,
                              model_name = "",
                              xlim = c(0, 60),
                              xbreaks = seq(0, 60, 6),
                              xlab = "Age (in months)",
                              label.trunc = 60,
                              col.manual = NULL,
                              na.value = "grey") {

  if (is.null(col.manual)) {
    stop("Specify col.manual")
  }

  data_plot <- pass |>
    filter(.data[[item_var]] == by_value, n >= min_n)

  if (!is.null(data_rug) && nrow(data_rug) > 0) {
    data_rug <- data_rug |>
      filter(.data[[item_var]] == by_value)
  }

  the_label <- data_plot$label[1]
  the_label <- substr(the_label, 1L, label.trunc)
  if (model_name != "") model_name <- paste("-", model_name)

  # Set colors
  if (!is.null(data_rug) && nrow(data_rug) > 0) {
    present_studies <- unique(data_rug[[by_var]])
  } else {
    present_studies <- unique(data_plot[[by_var]])
  }
  col.manual <- col.manual[names(col.manual) %in% present_studies]
  missing_studies <- setdiff(present_studies, names(col.manual))
  if (length(missing_studies) > 0L) {
    warning(
      "No color assigned for: ",
      paste(shQuote(missing_studies), collapse = ", ")
    )
  }

  # Initialize plot
  plot <- ggplot(
    data = data_plot,
    mapping = aes(
      x = .data$a,
      y = .data$p,
      group = !!sym(by_var),
      colour = !!sym(by_var)
    )
  ) +
    scale_x_continuous(
      name = xlab,
      limits = xlim,
      breaks = xbreaks
    ) +
    scale_y_continuous(
      name = "% pass",
      limits = c(0, 100),
      breaks = seq(0, 100, 20)
    ) +
    scale_colour_manual(
      values = col.manual,
      na.value = na.value
    )

  # Add rugs
  if (!is.null(data_rug) && nrow(data_rug) > 0) {
    plot <- plot +
      geom_rug(
        data = filter(data_rug, .data[[response_var]] == 0),
        mapping = aes(
          x = !!sym(x_var),
          y = !!sym(response_var),
          group = !!sym(by_var),
          colour = !!sym(by_var)
        ),
        position = "jitter",
        sides = "b",
        size = 0.2
      ) +
      geom_rug(
        data = filter(data_rug, .data[[response_var]] == 1),
        mapping = aes(
          x = !!sym(x_var),
          y = !!sym(response_var),
          group = !!sym(by_var),
          colour = !!sym(by_var)
        ),
        position = "jitter",
        sides = "t",
        size = 0.2
      )
  }

  # Add proportions if data is available
  if (nrow(data_plot) > 0) {
    plot <- plot +
      geom_line() +
      geom_point()
  }

  # Base annotations and theming
  plot <- plot +
    theme(
      legend.position = c(0.99, 0.05),
      legend.justification = c(1, 0),
      legend.key.size = unit(3.0, "mm"),
      legend.spacing.y = unit(0.5, "mm"),
      legend.background = element_rect(
        fill = "transparent",
        colour = "transparent"
      )
    ) +
    guides(
      fill = guide_legend(title = NULL),
      col = guide_legend(ncol = 1)
    ) +
    annotate(
      "text",
      x = 1, y = 7, hjust = 0,
      label = paste0(i, "  ", by_value),
      family = "Courier",
      fontface = "bold"
    )

  # Add item label if available
  if (!is.na(the_label)) {
    plot <- plot +
      annotate(
        "text",
        x = 1, y = 2, hjust = 0,
        label = the_label
      )
  }

  return(plot)
}
