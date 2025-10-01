#' Calculate the D-score model object given a fitted Rasch model
#'
#' This function calculates ability estimates and diagnostics from
#' a fitted Rasch model. It prepares the item bank, calculates
#' the corresponding D-score and DAZ, and computes fit statistics
#' for items and persons. It also handles equating and anchoring for
#' the D-score transformation. Results can be returned in either the logit
#' or in the D-score metric.
#'
#' @inheritParams rasch
#' @param fit An object produced by \code{\link{rasch}}.
#' @param name If specified, the string will be appended to
#' "number of items"_"number of equates".
#' @param key A string with a provisional key name. If not specified, the
#' \code{name} is used.
#' @param anchors A named numeric vector of length 2. The names of the vector
#' must correspond to two item names in `data` that will act as anchors
#' to define the scale units. If specified, the function will update
#' both `tau` and `transform` parameters. The default is `anchors = NULL`, which
#' means that the `tau` is calculated through the `transform` argument.
#' @param transform A vector of length 2 with the intercept and slope of the
#' linear transformation of item difficulties `tau` to the D-score scale.
#' The default c(0, 1) makes all calculations in the logit scale.
#' @param itemtable_from Location of the `itemtable` for the items.
#' The default `NULL` selects `dscore::builtin_itemtable`.
#' Setting `itemtable_from` to `""` creates simple sequential labels.
#' Setting `itemtable_from` to `ddata::itemtable` selects the itemtable
#' object in the `ddata` package. Note that items not listed in `itemtable`
#' will not '' appear in the `itembank`.
#' @param relevance See \code{\link[dscore]{dscore}}.
#' @param population Name of reference to calculate DAZ: See dscore()
#' @param prior_mean Name of prior mean column: See dscore()
#' @param background_equate A named list or a logical. The named list
#' has the same structure as the `equate` parameter. The argument specifies a
#' list of all (active and passive) equate groups. Each element specifies
#' a vector of item names belonging to the same equate group. Setting
#' `background_equate = TRUE` calculates the parameter
#' from `dscore::builtin_itemtable`. The function adds new equate groups
#' and updates existing equate groups by the corresponding
#' `background_equate`, or overwrite an existing element along
#' the `equate` object for active equates. Setting
#' `background_equate = FALSE` (default) does not specify any passive
#' equates.
#' @return An object of class \code{dmodel}. See details for a description
#' of the list elements.
#' @examples
#' \dontrun{
#' library("dmetric")
#' library("tidyr")
#'
#' # test with Dutch items
#' varlist <- prepare_items(study = "Netherlands 1")
#' dmo <- fit_dmodel(varlist, name = "Dutch")
#' }
#' @export
calculate_dmodel <- function(data,
                             fit,
                             anchors = NULL,
                             name = NULL,
                             key = NULL,
                             transform = c("(Intercept)" = 0, "original" = 1),
                             itemtable_from = NULL,
                             relevance = c(-Inf, Inf),
                             population = NULL,
                             prior_mean = NULL,
                             background_equate = FALSE) {
  call <- match.call()
  items <- fit$items

  # construct a name for the model
  name <- paste(length(fit$items), length(fit$equate), name, sep = "_")
  if (is.null(key)) key <- name

  # transform to D-score scale using two anchor items
  if (!is.null(anchors)) {
    # update both tau and transform
    tau <- set_anchor(difficulty = -fit$betapar,
                      values = anchors, items = names(anchors))
    transform <- calculate_transform(original = -fit$betapar, transformed = tau)
  } else {
    if (transform == "auto") {
      # auto: approximate published key
      tau_new <- -fit$betapar
      tau_old <- get_tau(items = names(tau_new), key = "gsed2406")
      # remove item Runs Well because that obtained an incorrect tau in Phase 1
      tau_new <- tau_new[!names(tau_new) %in% "gl1gmd036"]
      tau_old <- tau_old[!names(tau_old) %in% "gl1gmd036"]
      calib <- lm(tau_old ~ tau_new, data.frame(tau_new = tau_new, tau_old = tau_old))
      transform <- coef(calib)
    } else if (is.numeric(transform) && length(transform) == 2L) {
      transform <- calculate_transform(original = -fit$betapar, transformed = tau)
    } else if (length(transform) != 2L) {
      stop("The 'transform' argument must be a vector of length 2.")
    }
    # update only tau
    tau <- transform[1L] + transform[2L] * -fit$betapar
  }

  # itembank with delta's
  model_equatelist <- bind_equates(equate = fit$equate,
                                   background_equate = background_equate,
                                   items = items)
  model_itemtable <- calculate_itemtable(items = items,
                                         equatelist = model_equatelist,
                                         itemtable  = itemtable_from,
                                         activenames = names(fit$equate))
  itembank <- calculate_itembank(key = key,
                                 tau,
                                 itemtable = model_itemtable)

  # convert to wide format, for calling dscore()
  if (fit$shape == "long") {
    wide <- data |>
      pivot_wider(
        id_cols = all_of(c(fit$visit_var, "agedays")),
        names_from = all_of(fit$item_var),
        values_from = all_of(fit$response_var))
  } else {
    wide <- data
  }

  # key and ability in dscore scale
  betad <- dscore(data = wide, items = items,
                  xname = "agedays", xunit = "days",
                  key = key, population = population,
                  itembank = itembank, transform = transform,
                  metric = "dscore",
                  prior_mean = prior_mean, relevance = relevance)
  df_d <- data.frame(wide[, fit$visit_var], betad)

  # ability in logit scale
  beta <- dscore(data = wide, items = items,
                 xname = "agedays", xunit = "days",
                 key = key, population = population,
                 itembank = itembank, transform = transform,
                 metric = "logit",
                 prior_mean = prior_mean, relevance = relevance)
  df_b <- data.frame(wide[, fit$visit_var], beta)

  # fit statistics
  fs <- calculate_fit_statistics(data = data, items = items, fit = fit,
                                 itembank = itembank, ability = df_d,
                                 transform = transform)
  # store model
  model <- list(name = name,
                # data = data,
                equate = fit$equate,
                items = items,
                fit = fit,
                beta_l = df_b,
                anchors = anchors,
                transform = transform,
                itemtable = model_itemtable,
                itembank = itembank,
                dscore = df_d,
                item_fit = fs$item_fit,
                person_fit = fs$person_fit,
                equate_fit = fs$equate_fit,
                residuals = fs$residuals,
                active_equates = names(fit$equate),
                call = call,
                date = Sys.time())
  class(model) <- "dmodel"
  return(model)
}
