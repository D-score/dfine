#' Calculates the cohort from the subject ID
#'
#' @param subjid A vector with with subject IDs.
#' @return A vector of the same length with `cohort` name
#' @examples
#' id <- c("050-GSED-12345", "156-GSED-32211", "076-GSED-123")
#' calculate_cohort(id)
#' @export
calculate_cohort <- function(subjid) {

  isonum <- as.integer(stri_extract_first_regex(subjid, "^\\d{3}"))
  ctry <- case_when(
        isonum == 50L  ~ "BGD",
        isonum == 586L ~ "PAK",
        isonum == 834L ~ "TZA",
        isonum == 76L  ~ "BRA",
        isonum == 156L ~ "CHN",
        isonum == 384L ~ "CIV",
        isonum == 528L ~ "NLD",
        TRUE ~ NA_character_)
  cohort <- paste0("GSED-", ctry)

  return(cohort)
}
