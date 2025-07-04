#' Retrieve taxon key from GBIF
#'
#' Use rgbif to find the best match for the name given and return the taxon key
#' @param query The search parameter to pass to `rgbif::name_suggest()`
#' @param rank The taxonimic rank associated with the search parameter
#' @return The taxon key that GBIF needs to search its database for occurrence 
#' records
#' @examples 
#' # Each of these examples reflect a different taxonomic rank
#' # Only the first will run with "Run Examples" to limit GBIF API calls
#' key <- get_key(query = "Phelsuma", rank = "genus")
#' \dontrun{
#' key <- get_key(query = "Tamias ruficaudus ruficaudus", rank = "subspecies")
#' key <- get_key(query = "Naesiotus akamatus", rank = "species")
#' key <- get_key(query = "Araneae", rank = "order")
#' }
#' 
#' @export
get_key <- function(query, rank) {
  # Checkmate input validation
  checkmate::assertString(query)
  checkmate::assertString(rank)
  
  suggestions <- rgbif::name_suggest(q = query, rank = rank)
  # name_suggest orders by relevance, so pick the first
  if(length(suggestions$data) > 0){
    key <- as.numeric(suggestions$data[1,1])
  } else {
    key <- NA
  }
  
  # If the name is not an exact match, tell the user
  # Make sure suggestions is not empty before attempting
  if(length(suggestions$data) > 0 && query != suggestions$data[1,2]){
    suggested_name <- suggestions$data[1,2]
    suggested_rank <- suggestions$data[1,3]
    cli::cli_alert_info("No exact match found for query. 
                   The closest match was {suggested_name}, which has the following rank: {suggested_rank}.")
  }
  
  return(key)
}
