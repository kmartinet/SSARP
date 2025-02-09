% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpeARP.R
\name{SpeARP}
\alias{SpeARP}
\title{Create a speciation-area relationship plot}
\usage{
SpeARP(occurrences, npsi = 2, MS = FALSE)
}
\arguments{
\item{occurrences}{The dataframe output by one of SSARP's speciation methods (speciationDR), or if using a custom dataframe, ensure that it has the following columns: areas, rate}

\item{npsi}{The maximum number of breakpoints to estimate for model selection.  Default: 2}

\item{MS}{(Logical) Whether you used the speciationMS function to calculate speciation rates (TRUE) or not (FALSE - default). The speciationMS function already calculates a log-transformed speciation rate, so this prevents the rate from being log-transformed twice.}
}
\value{
A list of 3 including: the summary output, the segmented regression object, and the aggregated dataframe used to create the plot
}
\description{
Use segmented regression to create a speciation-area relationship plot. The X axis represents log(island area) and the Y axis represents log(speciation rate)
}
\examples{
\dontrun{
seg <- SpeARP(occurrences)
summary <- seg[1]
}
}
