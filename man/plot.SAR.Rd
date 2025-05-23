% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.SAR.R
\name{plot.SAR}
\alias{plot.SAR}
\title{Plot a species-area relationship}
\usage{
\method{plot}{SAR}(x, ...)
}
\arguments{
\item{x}{The SAR object that will be plotted}

\item{...}{Functions to pass to plot()}
}
\value{
A plot of your species-area relationship
}
\description{
Function for plotting species-area relationship objects from the
SSARP::SARP() function
}
\examples{
\dontrun{
seg <- SARP(occurrences)
plot(seg)
}
}
