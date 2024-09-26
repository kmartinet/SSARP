#' Get occurrence data from GBIF
#' 
#' Use rgbif to retrieve occurrence data from GBIF's database for a given taxon. This function will only return occurrences that include GPS coordinates.
#' @param key The taxon key for the taxon of interest (can be found using SSARP::getKey)
#' @param limit The maximum number of occurrence records to return. Default: 100. NOTE: there is a hard maximum of 100,000 (see more information about the limit in rgbif::occ_search)
#' @return A dataframe of occurrence records for the taxon of interest
#' @examples 
#' \dontrun{
#' dat <- getData(key = 2447418, limit = 1000)
#' }
#' @export

getData <- function(key, limit = 100) {
  occurrences <- occ_search(taxonKey = key, hasCoordinate = TRUE, limit = limit)
  return(occurrences$data)
}
