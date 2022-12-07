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

get_key <- function(query, rank) {
  suggestions <- name_suggest(q = query, rank = rank)
  # name_suggest orders by relevance, so pick the first
  key <- as.numeric(suggestions$data[1,1])
  return(key)
}