% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_cohort.R
\name{calculate_cohort}
\alias{calculate_cohort}
\title{Calculates the cohort from the subject ID}
\usage{
calculate_cohort(subjid)
}
\arguments{
\item{subjid}{A vector with with subject IDs.}
}
\value{
A vector of the same length with \code{cohort} name
}
\description{
Calculates the cohort from the subject ID
}
\examples{
id <- c("050-GSED-12345", "156-GSED-32211", "076-GSED-123")
calculate_cohort(id)
}
