#' Retrieve taxon key from GBIF
#'
#' Use rgbif to find the best match for the name given and return the taxon key
#' @param query The search parameter to pass to rgbif's name_suggest() function
#' @param rank The taxonimic rank associated with the search parameter
#' @return The taxon key that GBIF needs to search its database for occurrence records
#' @examples 
#' key <- get_key("Phelsuma", "genus")
#' key <- get_key("Tamias ruficaudus ruficaudus", "subspecies")
#' key <- get_key("Naesiotus akamatus", "species")
#' key <- get_key("Araneae", "order")
#' @export

getKey <- function(query, rank) {
  suggestions <- name_suggest(q = query, rank = rank)
  # name_suggest orders by relevance, so pick the first
  key <- as.numeric(suggestions$data[1,1])
  return(key)
}


#' Get occurrence data from GBIF
#' 
#' Use rgbif to retrieve occurrence data from GBIF's database for a given taxon. This function will only return occurrences that include GPS coordinates.
#' @param key The taxon key for the taxon of interest (can be found using SSARP::get_key)
#' @param limit The maximum number of occurrence records to return. Default: 100. NOTE: there is a hard maximum of 100,000 (see more information about the limit in rgbif::occ_search)
#' @return A dataframe of occurrence records for the taxon of interest
#' @examples 
#' dat <- get_data(key = 2447418, limit = 1000)
#' @export

getData <- function(key, limit = 100) {
  occurrences <- occ_search(taxonKey = key, hasCoordinate = TRUE, limit = limit)
  return(occurrences$data)
}