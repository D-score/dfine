#' Obtains color palettes from a data package
#'
#' This function returns one of built-in color palettes from a
#' specified data package.
#'@param palet The name of the list component in the built-in
#'  color palette list \code{gsed_palettes}. Currently implemented
#'  are \code{"cohort"}, \code{"study"}, \code{"cohort"}, \code{"country"},
#'  \code{"domain"}, \code{"wave"} and \code{"instrument"}.
#'@param na_color The color that is return if not found. The default is
#'  \code{"grey"}.
#'@return The selected color palette. If not found, it returns the single
#'  color specified by \code{na_color}.
#'@examples
#' \dontrun{
#'  get_palette("domain")
#'  }
#'@export
get_palette <- function(
  palet = c("cohort", "study", "country", "domain", "instrument", "wave"),
  na_color = "grey"
) {
  palet <- match.arg(palet)
  utils::data("gsed_palettes", package = "dfine")
  p <- get0("gsed_palettes", mode = "list")
  if (is.null(p)) {
    return(grDevices::rgb(t(grDevices::col2rgb(na_color)), maxColorValue = 255))
  }
  return(p[[palet]])
}
