#' Quickly create a species-area relationship
#' 
#' Use the SSARP workflow for creating a species-area relationship for island 
#' species using occurrence data from GBIF without having to go through every 
#' individual step.
#' @param taxon The name of the taxon of interest.
#' @param rank The taxonomic rank associated with the taxon of interest.
#' @param limit The number of GBIF occurrence records to obtain.
#' @param geometry (character) A polygon written in Well Known Text (WKT) 
#' format to pass to the `SSARP::get_data()` function. If no polygon is given, 
#' records from across the globe will be returned.
#' @param continent (logical) If TRUE, continental areas are removed from the 
#' species-area relationship data. If FALSE, continental areas are not removed 
#' from the species-area relationship data. Default: TRUE
#' @param npsi The number of breakpoints to estimate. Default: 1
#' @return A list of 3 including: the summary output, the segmented regression 
#' object, and the aggregated dataframe used to create the plot.
#' @examples 
#' seg <- quick_create_SAR(taxon = "Phelsuma", rank = "genus", limit = 100, npsi = 0)
#' 
#' @export

quick_create_SAR <- function(taxon, rank, limit = 100, geometry = NULL, 
                      continent = TRUE, npsi = 1) {
  # Get the taxon key from GBIF
  cli::cli_alert_info("Finding taxon key")
  key <- get_key(taxon, rank)
  
  cli::cli_alert_info("Gathering data")
  # Use the key to get data
  dat <- get_data(key, limit = limit, geometry = geometry)
  
  cli::cli_alert_info("Finding land")
  # Find the name of the land on which the occurrence points were found
  land <- find_land(dat)
  
  cli::cli_alert_info("Gathering areas")
  # Find the areas of the land
  areas <- find_areas(land)
  
  if(continent == TRUE) {
    cli::cli_alert_info("Removing continental areas")
    # Remove continents from the dataframe
    areas <- remove_continents(areas)
  }
  
  # Create the species-area relationship plot
  SAR_obj <- create_SAR(areas, npsi)
  
  return(SAR_obj)
  
}
