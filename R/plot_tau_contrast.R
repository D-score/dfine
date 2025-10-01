#' Plot tau contrast between two models
#'
#' @param model A `dmodel` object containing the new itembank and item_fit.
#' @param model_old A `dmodel` object containing the old itembank and item_fit.
#' @param detrended Logical, if TRUE, plot the difference in tau values.
#' @param ylim Numeric vector of length 2, specifying the y-axis limits.
#' @param colors Character vector of colors for the item categories.
#' @param fontcolor Character, color for the hover label font.
#' @return A plotly object representing the tau contrast plot.
#' @export
plot_tau_contrast <- function(model,
                              model_old,
                              detrended = TRUE,
                              ylim = c(-10, 10),
                              colors = c("#0072B2", "#D55E00", "#999999"),
                              fontcolor = "white") {

  if (!inherits(model, "dmodel") || !inherits(model_old, "dmodel")) {
    stop("Both model and model_old must be of class 'dmodel'.")
  }

  y_var <- if (detrended) rlang::sym("tau_diff") else rlang::sym("tau_new")

  items <- model$itembank$item
  itembank_old <- model_old$itembank |>
    dplyr::mutate(
     item = dplyr::recode(  # we need this because old itembank may contain this reversed item
       .data$item,
       gpaclc088 = "gpaclc089",
       gpasec089 = "gpasec088"),
      item = rename_vector(.data$item, lexin = "gsed2", lexout = "gsed3")) |>
    dplyr::filter(.data$item %in% items)
  items_old <- itembank_old$item
  items <- intersect(items, items_old)
  itembank_old <- itembank_old[match(items, items_old), ]
  itembank <- model$itembank |>
    dplyr::filter(.data$item %in% items)
  itemfit <- model$item_fit |>
    dplyr::filter(.data$item %in% items)

  taus <- data.frame(
    item = itembank$item,
    tau_new = itembank$tau,
    tau_old = itembank_old$tau,
    tau_diff = itembank$tau - itembank_old$tau,
    infit = itemfit$infit,
    outfit = itemfit$outfit,
    label = strtrim(itembank$label, 60))

  # Assign colors based on item prefix
  taus$color <- ifelse(
    startsWith(taus$item, "gs1"), colors[1L],
    ifelse(startsWith(taus$item, "gl1"), colors[2L], colors[3L])
  )

  taus$tooltip <- paste0(
    "<b>Item:</b> ", taus$item, "<br>",
    "<b>Label:</b> ", taus$label, "<br>",
    "<b>tau_old:</b> ", round(taus$tau_old, 3), "<br>",
    "<b>tau_new:</b> ", round(taus$tau_new, 3), "<br>",
    "<b>tau_dif:</b> ", round(taus$tau_diff, 3), "<br>",
    "<b>infit: <b> ", round(taus$infit, 3), "<br>",
    "<b>outfit:<b> ", round(taus$outfit, 3)
  )

  ref_line <- if (detrended) {
    list(
      # Light green band from -2 to +2
      list(
        type = "rect",
        xref = "x",
        yref = "y",
        x0 = min(taus$tau_old, na.rm = TRUE),
        x1 = max(taus$tau_old, na.rm = TRUE),
        y0 = -2,
        y1 = 2,
        fillcolor = "#DDDDDD",
        opacity = 0.3,
        line = list(width = 0)
      ),
      # Horizontal line at y = 0
      list(
        type = "line",
        x0 = min(taus$tau_old, na.rm = TRUE),
        x1 = max(taus$tau_old, na.rm = TRUE),
        y0 = 0,
        y1 = 0,
        line = list(color = "grey", width = 1)
      )
    )
  } else {
    rng <- range(c(taus$tau_old, taus$tau_new), na.rm = TRUE)
    list(
      list(
        type = "line",
        x0 = rng[1],
        x1 = rng[2],
        y0 = rng[1],
        y1 = rng[2],
        line = list(color = "grey", width = 1)
      )
    )
  }

  fig <- plotly::plot_ly(
    data = taus,
    x = ~tau_old,
    y = eval_tidy(sym(y_var), data = taus),
    type = 'scatter',
    mode = 'markers',
    text = ~tooltip,
    hoverinfo = 'text',
    marker = list(size = 8, color = ~ color)) |>
    plotly::layout(
      title = paste("tau", model$name),
      xaxis = list(title = model_old$name),
      yaxis = list(
        title = if (detrended) "tau difference" else "tau new",
        range = if (detrended) ylim else NULL
      ),
      shapes = ref_line,
      hoverlabel = list(
        font = list(color = fontcolor)
      )
    )
  return(fig)
}
