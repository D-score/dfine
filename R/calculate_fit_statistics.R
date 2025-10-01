#' Calculate fit statistics for a Rasch solution
#'
#' This function calculates three types of residual fit statistics: item
#' fit, person fit and equate fit. Both infit and outfit is calculated, as well
#' as their standardized form.
#'
#' Outfit is equivalent to the unweighted mean square. Infit is equivalent
#' to weighted mean square. See Wright and Masters (1982) p. 100 for the
#' relevant formula's.
#' @inheritParams rasch
#' @inheritParams calculate_dmodel
#' @param itembank The itembank with item difficulties, usually calculated
#' by \code{calculate_itembank()}
#' @param ability A data frame, with admin variables and a column b for ability calculated by
#' \code{dscore()}.
#' @return A `list` with elements:
#'
#' 1. item_fit
#' 2. person_fit
#' 3. equate_fit
#' 4. residuals
#' @references
#' Wright, B.D. and Masters, G.N. (1982). Rating Scale Analysis. Chicago,
#' MESA Press.
#'
#' Van Buuren, S. (2014). Growth charts of human development.
#' Stat Methods Med Res.;23:346-368.
#' @noRd
calculate_fit_statistics <- function(
  data,
  items,
  fit,
  itembank,
  ability,
  transform = c(0, 1)
) {
  if (!"equate" %in% colnames(itembank)) {
    itembank$equate <- NA
  }

  # check if items are in data
  visit_var <- if (is.null(fit$visit_var)) {
    c("subjid", "agedays")
  } else {
    fit$visit_var
  }
  item_var <- if (is.null(fit$item_var)) "item" else fit$item_var
  response_var <- if (is.null(fit$response_var)) {
    "response"
  } else {
    fit$response_var
  }
  ability_var <- if (is.null(fit$ability_var)) "b" else fit$ability_var

  # calculate residuals
  if (fit$shape == "wide") {
    residuals <- data |>
      select(one_of(c(visit_var, items))) |>
      left_join(ability, by = visit_var) |>
      select(one_of(c(visit_var, items, ability_var))) |>
      pivot_longer(
        names_to = item_var,
        values_to = response_var,
        cols = -c(visit_var, ability_var),
        values_drop_na = TRUE
      )
  } else if (fit$shape == "long") {
    residuals <- data |>
      left_join(ability, by = visit_var) |>
      select(one_of(c(visit_var, item_var, response_var, ability_var))) |>
      drop_na(response_var)
  } else {
    stop("Unknown shape: ", fit$shape)
  }

  residuals <- residuals |>
    left_join(itembank, by = item_var) |>
    select(one_of(c(
      visit_var,
      "equate",
      item_var,
      response_var,
      "tau",
      ability_var
    ))) |>
    drop_na(all_of(ability_var)) |>
    mutate(
      taut = (.data$tau - transform[1L]) / transform[2],
      dt = (.data$d - transform[1L]) / transform[2],
      p = plogis(.data$dt, location = .data$taut),
      psi = exp(.data$dt - .data$taut),
      pi = .data$psi / (1 + .data$psi)
    )

  residuals <- residuals |>
    mutate(
      w = pmax(.data$p^2 * (1 - .data$p) + (1 - .data$p)^2 * .data$p, 0.01),
      c = pmax(.data$p^4 * (1 - .data$p) + (1 - .data$p)^4 * .data$p, 0.01),
      y = .data[[response_var]] - .data$p,
      z = .data$y / .data$w^0.5,
      z2 = .data$z^2,
      y2 = .data$w * .data$z2,
      w2 = .data$w^2,
      cdivw2 = .data$c / .data$w2,
      cminw2 = .data$c - .data$w2
    )

  # fit statistics per item
  item_fit <- residuals |>
    group_by(.data$item) |>
    summarize(
      n = n(),
      outfit = mean(.data$z2, na.rm = TRUE),
      qo = sqrt(pmin(sum(.data$cdivw2) / .data$n^2 - (1 / .data$n), 2)),
      outfit_z = (.data$outfit^(1 / 3) - 1) * (3 / .data$qo) + .data$qo / 3,
      infit = sum(.data$y2) / sum(.data$w),
      qi = sqrt(pmin(sum(.data$cminw2) / sum(.data$w)^2, 2)),
      infit_z = (.data$infit^(1 / 3) - 1) * (3 / .data$qi) + .data$qi / 3
    )

  # fit statistics per person-age
  person_fit <- residuals |>
    group_by(across(all_of(visit_var))) |>
    summarize(
      n = n(),
      outfit = mean(.data$z2),
      qo = sqrt(pmin(sum(.data$cdivw2) / .data$n^2 - (1 / .data$n), 2)),
      outfit_z = (.data$outfit^(1 / 3) - 1) * (3 / .data$qo) + .data$qo / 3,
      infit = sum(.data$y2) / sum(.data$w),
      qi = sqrt(pmin(sum(.data$cminw2) / sum(.data$w)^2, 2)),
      infit_z = (.data$infit^(1 / 3) - 1) * (3 / .data$qi) + .data$qi / 3
    )

  # fit statistics per equate
  if (is.null(fit$equate)) {
    equate_fit <- NULL
  }
  if (!is.null(fit$equate)) {
    equate_fit <- residuals |>
      filter(!is.na(.data$equate)) |>
      group_by(.data$equate) |>
      summarize(
        n = n(),
        outfit = mean(.data$z2),
        qo = sqrt(pmin(sum(.data$cdivw2) / .data$n^2 - (1 / .data$n), 2)),
        outfit_z = (.data$outfit^(1 / 3) - 1) * (3 / .data$qo) + .data$qo / 3,
        infit = sum(.data$y2) / sum(.data$w),
        qi = sqrt(pmin(sum(.data$cminw2) / sum(.data$w)^2, 2)),
        infit_z = (.data$infit^(1 / 3) - 1) * (3 / .data$qi) + .data$qi / 3
      )
  }

  # sort in proper order
  item_fit <- item_fit[order_itemnames(item_fit$item), ]
  equate_fit <- equate_fit[stri_order(equate_fit$equate, numeric = TRUE), ]

  return(list(
    item_fit = item_fit,
    person_fit = person_fit,
    equate_fit = equate_fit,
    residuals = select(residuals, all_of(c(visit_var, item_var, "p", "y", "z")))
  ))
}
