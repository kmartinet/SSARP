% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findLand.R
\name{findLand}
\alias{findLand}
\title{Find the name of the land on which the occurrence points were found}
\usage{
findLand(occurrences, fillgaps = FALSE)
}
\arguments{
\item{occurrences}{The dataframe output by getData (or if using a custom
dataframe, ensure that it has the following columns: decimalLongitude,
decimalLatitude, acceptedScientificName, genericName, specificEpithet,
datasetKey). The "datasetKey" column is important for GBIF records and
identifies the dataset to which the occurrence record belongs. Custom
dataframes without this style of data organization should fill the column
with placeholder values.}

\item{fillgaps}{(logical) Attempt to use Photon API to fill in gaps left by
mapdata::map.where (TRUE) or only mapdata::map.where results
(FALSE, default). While it is powerful, the Photon API does not have a
standard location for island names in its returned information, so using it
will likely require the returned dataframe to be cleaned by the user.}
}
\value{
A dataframe of the species name, longitude, latitude, and three parts
of occurrence information. "First" is the name used to describe the largest
possible area of land where the occurrence point is found. "Second" is the
name used to describe the second-largest possible area of land that
corresponds with the occurrence point. "Third" is the most specific area of
land that corresponds with the occurrence point. Functions later in the SSARP
pipeline default to checking whether "Third" has an entry, then look at
"Second," and then "First."
}
\description{
Use various mapping tools to attempt to find the names of land masses where
the occurrence points were found.
}
\examples{
\dontrun{
key <- getKey(query = "Anolis", rank = "genus")
dat <- getData(key = key, limit = 100)
occs <- findLand(occurrences = dat, fillgaps = FALSE)}
}
