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
