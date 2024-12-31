#' Find areas of land masses.
#' 
#' Reference a dataset of island names and areas to find the areas of the land masses relevant to the taxon of interest.
#' @param occs The dataframe that is returned by SSARP::findLand. If using a custom occurrence record dataframe, ensure that it has the following columns in order: "SpeciesName", "Genus", "Species", "Longitude", "Latitude", "First", "Second", "Third"
#' @param area_custom (Optional) A dataframe including names of land masses and their associated areas. This dataframe should be provided when the user would like to bypass using the built-in database of island names and areas. Please ensure that the custom dataframe includes the land mass's area in column 3 and the name in column 5.
#' @return A dataframe of the species name, island name, and island area
#' @examples 
#' \dontrun{areas <- findAreas(occs)}
#' @export

findAreas <- function(occs, area_custom = NULL) {
  # Remove rows where First, Second, and Third are all NA
  # Create vector to hold row numbers
  minus <- rep(NA, nrow(occs))
  # Loop through dataframe
  for(i in c(1:nrow(occs))){
    if(nrow(occs) == 0){
      print("No data in occurrence record dataframe")
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
  
  # For each island name in the current dataframe, find the area and add the pair to the dictionary
  
  # First, create an empty list of island names
  islands <- list()
  
  # Next, go through the occs dataframe and see if the Third column has a name in it
  # If yes, add to the island list. If NA, go to the Second column. If NA, go to the First column
  print("Recording island names...")
  for(i in c(1:nrow(occs))) {
    if(nrow(occs)==0){
      print("No data in occurrence record dataframe")
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
  # If the user did not supply a custom dataframe, get island areas from built-in area file
  if(is.null(area_custom)){
    area_file <- SSARP::island_areas
  } else {
    area_file <- area_custom
  }
  
  
  # Look through the island area file and find the names in the uniq_islands list
  print("Assembling island dictionary...")
  for (i in c(1:length(uniq_islands))) {
    
    for(j in c(1:nrow(area_file))) {
      
      area_compare <- area_file[j,5]
      area_compare2 <- paste0(area_file[j,5], " Island")
      uniq_compare <- as.character(uniq_islands[i])
      uniq_compare2 <- paste0(as.character(uniq_islands[i]), " Island")
      
      if(is.na(uniq_compare)) {
        break
      }
      
      if(area_compare == uniq_compare || area_compare2 == uniq_compare || area_compare == uniq_compare2 || area_compare2 == uniq_compare2) {
        island_dict[as.character(uniq_islands[i])] <- area_file[j,3]
        break # Break the inner loop when you find the island name
        
      }
    }
  }
  
  # Use the dictionary to add the areas to the final dataframe
  print("Adding areas to final dataframe...")
  areas <- rep(0, times = nrow(occs))
  
  for(i in c(1:nrow(occs))) {
    
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
  
  return(occs_final)
  
}
