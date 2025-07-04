#' Access properly encoded island_area object
#' 
#' In order to address an R CMD check warning about non-ASCII characters in the 
#' island_area object, these characters in the island names had to be converted 
#' to an ASCII format. The non-ASCII accents in the island names are important 
#' for the functionality of the SSARP package, so this function provides the 
#' user with a dataframe including the original, un-converted island names.
#' @return An edited version of the `SSARP::island_areas` object, which is a 
#' dataframe including the names, areas, and maximum elevations of islands 
#' from across the globe.
#' @examples 
#' island_df <- get_island_areas()
#' @importFrom stringi stri_unescape_unicode
#' @export

get_island_areas <- function() {
  # Load island_areas object from package
  island_df <- SSARP::island_areas
  
  # Convert Unicode to original UTF-8 form
  island_df$Name <- stringi::stri_unescape_unicode(island_df$Name)

  # Return dataframe with newly converted names
  return(island_df)
}
