plot_pass_item <- function(
  pass,
  data_rug = NULL,
  item_var = "item",
  response_var = "response",
  x_var = "a",
  by_var = "cohort",
  by_value,
  i = 0,
  min_n = 10,
  model_name = "",
  xlim = c(0, 60),
  xbreaks = seq(0, 60, 6),
  xlab = "Age (in months)",
  label_trunc = 60,
  col_manual = NULL,
  na_value = "grey"
) {
  if (is.null(col_manual)) {
    stop("Specify col_manual")
  }

  data_plot <- pass |>
    filter(.data[[item_var]] == by_value, n >= min_n)

  if (!is.null(data_rug) && nrow(data_rug) > 0) {
    data_rug <- data_rug |>
      filter(.data[[item_var]] == by_value)
  }

  the_label <- data_plot$label[1]
  the_label <- substr(the_label, 1L, label_trunc)
  if (model_name != "") {
    model_name <- paste("-", model_name)
  }

  # Set colors
  if (!is.null(data_rug) && nrow(data_rug) > 0) {
    present_studies <- unique(data_rug[[by_var]])
  } else {
    present_studies <- unique(data_plot[[by_var]])
  }
  col_manual <- col_manual[names(col_manual) %in% present_studies]
  missing_studies <- setdiff(present_studies, names(col_manual))
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
      x = .data[[x_var]],
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
      values = col_manual,
      na.value = na_value
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
        linewidth = 0.2
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
        linewidth = 0.2
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
      x = 1,
      y = 7,
      hjust = 0,
      label = paste0(i, "  ", by_value),
      family = "Courier",
      fontface = "bold"
    )

  # Add item label if available
  if (!is.na(the_label)) {
    plot <- plot +
      annotate(
        "text",
        x = 1,
        y = 2,
        hjust = 0,
        label = the_label
      )
  }

  return(plot)
}
