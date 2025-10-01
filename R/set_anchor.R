#' Set the anchor of the D-score scale
#'
#' This function calculates the tranformation of difficulty estimates
#' in the logit to a scale with fixed left and right anchors, and returns the
#' transformed difficulty estimates.
#' @param difficulty Named numerical vector with difficulty estimates of the
#' items, in the logit scale.
#' @param items Character vector of length two with the names of the left
#' and right anchor item. The default is `c("gtogmd001", "gtogmd026")`, which
#' corresponds to the items: "Lifts head in prone 45 degrees" and
#' "Moves from lying to sitting"
#' @param values Numeric vector of length two with the labels of left
#' and right anchor item. The default is \code{(20, 40)}.
#' @return A vector with \code{length(beta)} elements with transformed
#' difficulty estimate
#' @details
#' This function scales the difficulty levels by a linear transformation
#' such that the difficulty levels of the two anchor items are set to
#' fixed values. The default setting produces the scale recommended in
#' Van Buuren (2014).
#' @references
#' van Buuren S (2014). Growth charts of human development. \emph{Statistical
#' Methods in Medical Research}, 23(4), 346-368.
#' @author Stef van Buuren, 2016
#' @examples
#' difficulty <- c(-1.5, 0, -0.4, 0.8, 1.2)
#' names(difficulty) <- paste0("item", 1:5)
#' set_anchor(difficulty, items = c("item2", "item4"))
#' @export
set_anchor <- function(
  difficulty,
  items = c("gtogmd001", "gtogmd026"),
  values = c(20, 40)
) {
  missing <- setdiff(items, names(difficulty))
  if (length(missing) > 0L) {
    stop("Anchor items not found: ", paste(missing, collapse = ", "))
  }
  df <- data.frame(y = NA, x = difficulty)
  df[items[1], "y"] <- values[1]
  df[items[2], "y"] <- values[2]
  fit <- lm(y ~ x, data = na.omit(df))
  return(predict(fit, newdata = df))
}
