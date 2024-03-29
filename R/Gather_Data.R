#' Retrieve taxon key from GBIF
#'
#' Use rgbif to find the best match for the name given and return the taxon key
#' @param query The search parameter to pass to rgbif's name_suggest() function
#' @param rank The taxonimic rank associated with the search parameter
#' @return The taxon key that GBIF needs to search its database for occurrence records
#' @examples 
#' \dontrun{
#' key <- getKey("Phelsuma", "genus")
#' key <- getKey("Tamias ruficaudus ruficaudus", "subspecies")
#' key <- getKey("Naesiotus akamatus", "species")
#' key <- getKey("Araneae", "order")
#' }
#' @import rgbif
#' @export

getKey <- function(query, rank) {
  suggestions <- name_suggest(q = query, rank = rank)
  # name_suggest orders by relevance, so pick the first
  if(length(suggestions$data)!= 0){
    key <- as.numeric(suggestions$data[1,1])
  } else {
    key <- NA
  }
  return(key)
}


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