#' Get occurrence data from GBIF
#' 
#' Use rgbif to retrieve occurrence data from GBIF's database for a given taxon. This function will only return occurrences that include GPS coordinates.
#' @param key The taxon key for the taxon of interest (can be found using SSARP::getKey)
#' @param limit The maximum number of occurrence records to return. Default: 100. NOTE: there is a hard maximum of 100,000 (see more information about the limit in rgbif::occ_search)
#' @param geometry A counter-clockwise winding order well-known text (WKT) representation of geometry for use in the rgbif::occ_search call. This parameter will restrict returned occurrence records to the specified geometry. More information about valid WKT objects can be found in the rgbif::occ_search vignette.
#' @return A dataframe of occurrence records for the taxon of interest
#' @examples 
#' \dontrun{
#' dat <- getData(key = 2447418, limit = 1000)
#' 
#' # With geometry that restricts returned Anolis data to a box around Florida
#' # (Includes some records in Georgia and the Biminis due to the size of the square)
#' dat <- getData(key = 8782549, 
#'               limit = 1000, 
#'               geometry = 'POLYGON((-87.4 24.3, -79.2 24.3, -79.2 31.1, -87.4 31.1, -87.4 24.3))')
#' }
#' @import rgbif
#' @export

getData <- function(key, limit = 100, geometry = NULL) {
  # If the user did not supply geometry, run occ_search without geometry
  if(is.null(geometry)){
    occurrences <- occ_search(taxonKey = key, hasCoordinate = TRUE, limit = limit)
  } else {
    # If the user supplied geometry, use it in the occ_search call
    occurrences <- occ_search(taxonKey = key, hasCoordinate = TRUE, limit = limit, geometry = geometry)
  }
  
  return(occurrences$data)
}
