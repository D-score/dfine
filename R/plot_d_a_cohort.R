#' Plot D-score against age by cohort
#'
#' This function plots D-score against age by cohort
#' @param data Data on which model was fitted
#' @param daz Logical indicating whether to plot D-score or DAZ.
#' @param show_smooth Logical indicating whether to show a smoothed line
#' @param file A filename to which the pdf is saved. If not specified, no
#' plot is saved.
#' @param col_manual A named vector of colors to use for the cohorts.
#' @param model_name Name of the model used for D-score calculation.
#' @param ref_name Name of reference to be plotted.
#' @param size Size of points in the plot.
#' @param shape Shape of points in the plot.
#' @param xlim Limits for the x-axis.
#' @param ylim Limits for the y-axis.
#' @param xbreaks Breaks for the x-axis.
#' @param ybreaks Breaks for the y-axis.
#' @param smooth_line_color Color of the smoothed line.
#' @param device	Device to use. Can be either be a device function (e.g. png()),
#'  or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp",
#'  "svg" or "wmf" (windows only).
#' @return A object of class \code{ggplot}
#' @export
plot_d_a_cohort <- function(
  data,
  daz = FALSE,
  show_smooth = FALSE,
  file = NULL,
  col_manual = NULL,
  model_name = NULL,
  ref_name = c("preliminary_standards", "phase1", "dutch", "gcdg", "none"),
  size = 0.5,
  shape = 1,
  xlim = c(0, 60),
  ylim = c(0, 90),
  xbreaks = seq(0, 60, 12),
  ybreaks = seq(0, 90, 20),
  smooth_line_color = "grey",
  device = "pdf"
) {
  ref_name <- match.arg(ref_name)

  # select data for reference layer
  if (ref_name %in% c("preliminary_standards", "none")) {
    reference <- get_reference(population = "preliminary_standards") |>
      mutate(month = .data$age * 12) |>
      select(.data$month, .data$SDM2:.data$SDP2) |>
      filter(.data$month <= 42) |>
      pivot_longer(names_to = "centile", values_to = "d", cols = -.data$month)
  }
  if (ref_name %in% c("phase1")) {
    reference <- get_reference(population = "phase1") |>
      mutate(month = .data$age * 12) |>
      select(.data$month, .data$SDM2:.data$SDP2) |>
      filter(.data$month <= 42) |>
      pivot_longer(names_to = "centile", values_to = "d", cols = -.data$month)
  }
  if (ref_name == "dutch") {
    reference <- get_reference(population = "dutch") |>
      mutate(month = .data$age * 12) |>
      select("month", .data$SDM2:.data$SDP2) |>
      filter(.data$month <= 30) |>
      pivot_longer(names_to = "centile", values_to = "d", cols = -.data$month)
  }
  if (ref_name %in% c("gcdg")) {
    reference <- get_reference(population = "gcdg") |>
      mutate(month = .data$age * 12) |>
      select(.data$month, .data$SDM2:.data$SDP2) |>
      filter(.data$month <= 60) |>
      pivot_longer(names_to = "centile", values_to = "d", cols = -.data$month)
  }
  if (daz) {
    references <- get_reference(population = ref_name) |>
      mutate(month = .data$age * 12, SDM2 = -2, SDP2 = 2) |>
      select(.data$month, .data$SDM2, .data$SDP2) |>
      filter(.data$month <= 60) |>
      pivot_longer(names_to = "centile", values_to = "d", cols = -.data$month)
  }

  data$age <- data$a * 12
  ylab <- "D-score ("
  if (daz) {
    data$d <- data$daz
    ylab <- "DAZ ("
    ylim <- ylim %||% c(-4, 4)
    ybreaks <- ybreaks %||% seq(-3, 3, 1)
  }
  color <- switch(
    ref_name,
    preliminary_standards = "skyblue",
    phase1 = "#C5EDDE",
    dutch = "grey",
    gcdg = "lightblue",
    none = "transparent"
  )

  plot <- ggplot(
    reference,
    aes(x = .data$month, y = .data$d, group = .data$centile)
  ) +
    scale_colour_manual(
      values = col_manual %||% get_palette("cohort"),
      na.value = "grey"
    ) +
    scale_x_continuous("Age (in months)", limits = xlim, breaks = xbreaks) +
    scale_y_continuous(
      paste0(ylab, model_name, ")"),
      breaks = ybreaks,
      limits = ylim
    ) +
    geom_line(colour = color) +
    geom_point(
      mapping = aes(
        x = .data$age,
        y = .data$d,
        group = .data$cohort,
        colour = .data$cohort
      ),
      data = data,
      size = size,
      shape = shape
    )

  if (show_smooth) {
    plot <- plot +
      geom_smooth(
        mapping = aes(
          x = .data$age,
          y = .data$d,
          group = .data$cohort,
          colour = .data$cohort
        ),
        data = data,
        method = "loess",
        se = FALSE,
        span = 0.75,
        color = smooth_line_color,
        linewidth = 1
      )
  }

  plot <- plot +
    facet_wrap(~ .data$cohort, ncol = 4) +
    theme(legend.position = "none")
  n_facets <- length(unique(data$cohort))
  n_rows <- ceiling(n_facets / 4)

  if (!is.null(file)) {
    ggsave(
      file,
      plot = plot,
      device = device,
      width = 7,
      height = (7 / 4) * n_rows
    )
  }
  invisible(plot)
}
