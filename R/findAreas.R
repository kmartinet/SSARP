#' Find areas of land masses.
#' 
#' Reference a dataset of island names and areas to find the areas of the land 
#' masses relevant to the taxon of interest.
#' @param occs The dataframe that is returned by SSARP::findLand. If using a 
#' custom occurrence record dataframe, ensure that it has the following columns 
#' in order: "SpeciesName", "Genus", "Species", "Longitude", "Latitude", 
#' "First", "Second", "Third", "datasetKey". The "datasetKey" column is 
#' important for GBIF records and identifies the dataset to which the occurrence
#' record belongs. Custom dataframes without this style of data organization 
#' should fill the column with placeholder values.
#' @param area_custom A dataframe including names of land masses and their 
#' associated areas. This dataframe should be provided when the user would like 
#' to bypass using the built-in database of island names and areas. Please 
#' ensure that the custom dataframe includes the land mass's area in column 3 
#' and the name in column 5. (Optional)
#' @return A dataframe of the species name, island name, and island area
#' @examples 
#' \dontrun{
#' key <- getKey(query = "Anolis", rank = "genus")
#' dat <- getData(key = key, limit = 100)
#' occs <- findLand(occurrences = dat)
#' areas <- findAreas(occs = occs)}
#' @importFrom cli cli_alert_info cli_alert_warning
#' @importFrom checkmate assertDataFrame
#' @import Dict
#' @export

findAreas <- function(occs, area_custom = NULL) {
  # checkmate input verification
  assertDataFrame(occs)
  
  # Remove rows where First, Second, and Third are all NA
  # Create vector to hold row numbers
  minus <- rep(NA, nrow(occs))
  # Loop through dataframe
  for(i in seq_len(nrow(occs))){
    if(nrow(occs) == 0){
      cli_alert_warning("No data in occurrence record dataframe")
      break
    }
    if(is.na(occs[i,8]) && is.na(occs[i,7]) && is.na(occs[i,6])) {
      minus[i] <- i
    }
  }
  # Remove NAs (from initialization) from row number vector
  minus <- minus[!is.na(minus)]
  
  # If all of minus is NA, that means that there are no rows to delete
  # Only delete rows when minus is not 0
  if(length(minus) != 0){
    occs <- occs[-minus,]
  }
  
  # Add a temporary key-value pair to initialize
  island_dict <- Dict$new(
    bloop = 108
  )
  
  # For each island name in the current dataframe, 
  # find the area and add the pair to the dictionary
  
  # First, create an empty list of island names
  islands <- list()
  
  # Next, go through the occs dataframe and see if the Third column has a name.
  # If yes, add to the island list. If NA, go to the Second column. 
  # If Second column is NA, go to the First column.
  cli_alert_info("Recording island names...")
  for(i in seq_len(nrow(occs))) {
    if(nrow(occs) == 0){
      cli_alert_warning("No data in occurrence record dataframe")
      break
    }
    if(!is.na(occs[i,8])) {
      islands[i] <- occs[i,8]
    } else if (!is.na(occs[i,7])){
      islands[i] <- occs[i,7]
    } else if (!is.na(occs[i,6])){
      islands[i] <- occs[i,6]
    }
  }
  
  # Next, eliminate duplicate entries in the list
  uniq_islands <- unique(islands)
  
  # Next, add the island names as keys and their corresponding areas as values
  # If the user did not supply a custom dataframe, get island areas from 
  # built-in island area dataset
  if(is.null(area_custom)){
    area_file <- get_island_areas()
  } else {
    area_file <- area_custom
  }
  
  
  # Look through the island area file and find the names in uniq_islands list
  cli_alert_info("Assembling island dictionary...")
  # Initialize vector of island names from island area dataset with 
  #  "Island" appended
  area_file_append <- paste0(area_file$Name, " Island")
  # Initialize grep statements as NA
  grep_res <- grep_res2 <- grep_res3 <- NA

  for(i in seq(uniq_islands)) {
    # Use grep for exact match in the area database
    # [1] picks the first match if the query gets multiple matches
    query <- paste0("^", as.character(uniq_islands[i]), "$")
    grep_res <- grep(query, area_file$Name)[1]
    
    if(!is.na(grep_res)){
      # If grep found a match, add it to island dictionary
      island_dict[as.character(uniq_islands[i])] <- area_file[grep_res,3]
    } else {
      # If it doesn't find the name directly from uniq_islands, try adding 
      #  "island" at the end
      query <- paste0("^", as.character(uniq_islands[i]), " Island$")
      grep_res2 <- grep(query, area_file$Name)[1]
      if(!is.na(grep_res2)){
        # If grep found a match, add it to island dictionary 
        island_dict[as.character(uniq_islands[i])] <- area_file[grep_res2,3]
      }
    } 
    
    # If it doesn't find the name from uniq_islands, look in area_file_append
    if(is.na(grep_res2)){
      query <- paste0("^", as.character(uniq_islands[i]), "$")
      grep_res3 <- grep(query, area_file_append)[1]
      if(!is.na(grep_res3)){
        # If grep found a match, add it to island dictionary 
        island_dict[as.character(uniq_islands[i])] <- area_file[grep_res3,3]
      }
    }
  }
  
  # Use the dictionary to add the areas to the final dataframe
  cli_alert_info("Adding areas to final dataframe...")
  areas <- rep(0, times = nrow(occs))
  
  for(i in seq_len(nrow(occs))) {
    
    if(!is.na(occs[i,8]) && island_dict$has(occs[i,8])){
      areas[i] <- island_dict$get(occs[i,8])
    } else if(!is.na(occs[i,7]) && island_dict$has(occs[i,7])){
      areas[i] <- island_dict$get(occs[i,7])
    } else if(!is.na(occs[i,6]) && island_dict$has(occs[i,6])){
      areas[i] <- island_dict$get(occs[i,6])
    } else {
      areas[i] <- NA
    }
  }
  
  # Create final dataframe
  occs_final <- cbind(occs, areas)
  
  # Remove rows with NA in area column
  occs_final <- occs_final[!is.na(occs_final$areas),]
  
  # Ensure areas are numeric
  occs_final$areas <- as.numeric(occs_final$areas)
  
  return(occs_final)
  
}
