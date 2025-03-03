#' Find location name and area for occurrence records using user-provided shape file
#' 
#' Reference user-provided shape file to find location names and areas for occurrence records. This function is primarily written for users who want to create species- and speciation-area relationships for organisms living on systems that would not be covered by the island database referenced by SSARP::findAreas.
#' @param occs The dataframe that is returned by SSARP::getData. If using a custom occurrence record dataframe, ensure that it has the following columns: "decimalLatitude", "decimalLongitude", "genericName", "specificEpithet", and "datasetKey". The "datasetKey" column is important for GBIF records and identifies the dataset to which the occurrence record belongs. Custom dataframes without this style of data organization should fill the column with placeholder values.
#' @param shapefile A shapefile (.shp) containing spatial information for the geographic locations of interest (e.g., habitat fragments, lakes, rivers)
#' @param names If the user would like to restrict which polygons in the shapefile are included in the returned occurrence record dataframe, they can be specified here as a vector. If the user does not provide a vector, all of the non-NA names in the shapefile will be included (as found in shapefile$name) (Optional)
#' @return A list including two objects: "occs" (a dataframe of the species name, polygon name, polygon area, and GBIF datasetKey (if applicable) for each occurrence record) and "not_included" (a vector of names from the original occurrence record dataframe that are not in the final one)
#' @examples 
#' \dontrun{
#' key <- getKey(query = "Anolis", rank = "genus")
#' dat <- getData(key = key, limit = 100)
#' 
#' occs <- findShapeAreas(occs = dat, shapefile = shapefile, names = NULL)
#' new_occs <- occs[[1]]}
#' @importFrom cli cli_alert_info cli_alert_warning
#' @importFrom checkmate assertDataFrame assertClass
#' @importFrom terra vect subset extract
#' @importFrom sf st_area st_as_sf
#' @importFrom cli cli_alert_info
#' @export

findShapeAreas <- function(occs, shapefile, names = NULL) {
  ## Add statement talking about what is dropped: check is.na output and find which species were not included in the final dataset. Also tell user which lakes/shapes were not used in the final dataset
  
  # checkmate input verification
  assertDataFrame(occs)
  assertClass(shapefile, "SpatVector")
  
  # First, remove any rows where the "specificEpithet" column is NA
  occs <- occs[!is.na(occs$specificEpithet),]
  
  # Next, create "Species" column for use in later steps of the SSARP workflow
  occs$Species <- paste(occs$genericName, occs$specificEpithet)
  
  # Save species list as a separate vector
  # (So we can tell the user what species were excluded from the original occs)
  orig_sp <- unique(occs$Species)
  
  # If the user input a "names" vector, use it to subset the SpatVector
  if(!is.null(names)){
    polygons <- terra::subset(shapefile, shapefile$name %in% names)
  } else {
    cli_alert_info("Using all names in the shapefile, this might extend processing time")
    # If the user did not input a "names" vector, use the full list of polygon names
    # If there are any NAs in shapefile$name, remove them
    all_names <- shapefile$name[!is.na(shapefile$name)]
    
    # Still subset the shapefile using these names, since NAs were removed
    polygons <- terra::subset(shapefile, shapefile$name %in% all_names)
  }
  
  # Assign areas (in m^2) to polygons
  ## NOTE: this was a problem when using the all names methodology
  polygons$areas <- st_area(st_as_sf(polygons))
  
  # Assign polygons based on the GPS coordinates in occs
  poly_dat <- terra::extract(polygons, data.frame(occs$decimalLongitude, occs$decimalLatitude))
  
  # Trim to only include important columns
  poly_dat <- poly_dat[,c("featurecla", "name", "areas")]
  
  # Add polygon info for each occurrence record to occs
  occs <- cbind(occs, poly_dat)
  
  # Remove rows with NA in areas, give user a warning
  occs_no_area <- occs[is.na(occs$areas),]
  cli_alert_warning("Removed {length(occs_no_area[,1])} occurrence records with no area")
  
  occs <- occs[!is.na(occs$areas),]
  
  # Create vector of species included now, to compare to the start
  new_sp <- unique(occs$Species)
  
  # Save vector of not included species to share with the user
  not_included <- orig_sp[!(orig_sp %in% new_sp)]
  
  if(length(not_included) != 0){
    cli_alert_warning("{length(not_included)} species from the original occurrence record dataframe were not included. Vector of names in the 'not_included' list element of the object returned by this function.")
  }
  
  # Ensure the areas column is numeric
  occs$areas <- as.numeric(occs$areas)
  
  # Return list including new occs dataframe and vector of species not included
  result <- list("occs" = occs, "not_included" = not_included)

  return(result)
}
