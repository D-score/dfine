% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_anchor.R
\name{set_anchor}
\alias{set_anchor}
\title{Set the anchor of the D-score scale}
\usage{
set_anchor(difficulty, items = c("gtogmd001", "gtogmd026"), values = c(20, 40))
}
\arguments{
\item{difficulty}{Named numerical vector with difficulty estimates of the
items, in the logit scale.}

\item{items}{Character vector of length two with the names of the left
and right anchor item. The default is \code{c("gtogmd001", "gtogmd026")}, which
corresponds to the items: "Lifts head in prone 45 degrees" and
"Moves from lying to sitting"}

\item{values}{Numeric vector of length two with the labels of left
and right anchor item. The default is \code{(20, 40)}.}
}
\value{
A vector with \code{length(beta)} elements with transformed
difficulty estimate
}
\description{
This function calculates the tranformation of difficulty estimates
in the logit to a scale with fixed left and right anchors, and returns the
transformed difficulty estimates.
}
\details{
This function scales the difficulty levels by a linear transformation
such that the difficulty levels of the two anchor items are set to
fixed values. The default setting produces the scale recommended in
Van Buuren (2014).
}
\examples{
difficulty <- c(-1.5, 0, -0.4, 0.8, 1.2)
names(difficulty) <- paste0("item", 1:5)
set_anchor(difficulty, items = c("item2", "item4"))
}
\references{
van Buuren S (2014). Growth charts of human development. \emph{Statistical
Methods in Medical Research}, 23(4), 346-368.
}
\author{
Stef van Buuren, 2016
}
