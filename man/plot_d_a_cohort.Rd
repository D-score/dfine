% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_d_a_cohort.R
\name{plot_d_a_cohort}
\alias{plot_d_a_cohort}
\title{Plot D-score against age by cohort}
\usage{
plot_d_a_cohort(
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
)
}
\arguments{
\item{data}{Data on which model was fitted}

\item{daz}{Logical indicating whether to plot D-score or DAZ.}

\item{show_smooth}{Logical indicating whether to show a smoothed line}

\item{file}{A filename to which the pdf is saved. If not specified, no
plot is saved.}

\item{col_manual}{A named vector of colors to use for the cohorts.}

\item{model_name}{Name of the model used for D-score calculation.}

\item{ref_name}{Name of reference to be plotted.}

\item{size}{Size of points in the plot.}

\item{shape}{Shape of points in the plot.}

\item{xlim}{Limits for the x-axis.}

\item{ylim}{Limits for the y-axis.}

\item{xbreaks}{Breaks for the x-axis.}

\item{ybreaks}{Breaks for the y-axis.}

\item{smooth_line_color}{Color of the smoothed line.}

\item{device}{Device to use. Can be either be a device function (e.g. png()),
or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp",
"svg" or "wmf" (windows only).}
}
\value{
A object of class \code{ggplot}
}
\description{
This function plots D-score against age by cohort
}
