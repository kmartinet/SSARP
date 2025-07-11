% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_key.R
\name{get_key}
\alias{get_key}
\title{Retrieve taxon key from GBIF}
\usage{
get_key(query, rank)
}
\arguments{
\item{query}{The search parameter to pass to \code{rgbif::name_suggest()}}

\item{rank}{The taxonimic rank associated with the search parameter}
}
\value{
The taxon key that GBIF needs to search its database for occurrence
records
}
\description{
Use rgbif to find the best match for the name given and return the taxon key
}
\examples{
# Each of these examples reflect a different taxonomic rank
# Only the first will run with "Run Examples" to limit GBIF API calls
key <- get_key(query = "Phelsuma", rank = "genus")
\dontrun{
key <- get_key(query = "Tamias ruficaudus ruficaudus", rank = "subspecies")
key <- get_key(query = "Naesiotus akamatus", rank = "species")
key <- get_key(query = "Araneae", rank = "order")
}

}
