#' Find the name of the land on which the occurrence points were found
#' 
#' Use various mapping tools to attempt to find the names of land masses where the occurrence points were found.
#' @param occurrences The dataframe output by getData (or if using a custom dataframe, ensure that it has the following columns: decimalLongitude, decimalLatitude, acceptedScientificName)
#' @param fillgaps (logical) Attempt to use Photon API to fill in gaps left by mapdata::map.where (TRUE) or only mapdata::map.where results (FALSE, default). While it is powerful, the Photon API does not have a standard location for island names in its returned information, so using it will likely require the returned dataframe to be cleaned by the user.
#' @return A dataframe of the species name, longitude, latitude, and three parts of occurrence information. "First" is the name used to describe the largest possible area of land where the occurrence point is found. "Second" is the name used to describe the second-largest possible area of land that corresponds with the occurrence point. "Third" is the most specific area of land that corresponds with the occurrence point. Functions later in the SSARP pipeline default to checking whether "Third" has an entry, then look at "Second," and then "First."
#' @examples 
#' \dontrun{occs <- findLand(occurrences, fillgaps = FALSE)}
#' @import mapdata
#' @import tidyverse
#' @import httr
#' @import Dict
#' @import usethis
#' @export

findLand <- function(occurrences, fillgaps = FALSE) {
  lon<-as.numeric(occurrences$decimalLongitude)
  lat<-as.numeric(occurrences$decimalLatitude)
  # Use map.where to find landmass names that correspond to GPS points
  # First, use world2Hires
  where <- maps::map.where(database="mapdata::world2Hires", x=lon, y=lat)
  # Then, use world just in case it can fill gaps
  where2 <- maps::map.where(database="world", x=lon, y=lat)
  
  # Next, combine the two where options (prioritize where2)
  for(i in c(1:length(where2))) {
    # Check if this row is NA
    if(is.na(where2[i])){
      # If it is NA, check if where is not NA
      if(!is.na(where[i])) {
        # If where is not NA, grab the name it found
        where2[i] <- where[i]
      }
    }
  }
  
  occs <- as.data.frame(cbind(occurrences$acceptedScientificName, lon, lat, where2))
  # Separate the where column into two separate columns - Country and Island
  # But sometimes there are three...
  occs <- occs %>% tidyr::separate(where2, c("First", "Second", "Third"), sep = ":")
  colnames(occs) <- c("Species", "Longitude", "Latitude", "First", "Second", "Third")
  
  if(fillgaps == TRUE) {
    # There might still be a lot of NA entries, so use Photon to try to fill in gaps

    for(i in c(1:nrow(occs))) {
      if(is.na(occs[i,4])){
        # Get lon and lat
        longitude = occs[i,4]
        latitude = occs[i,3]
        
        # Create Photon URL
        url <- paste0("http://photon.komoot.io/reverse?lon=", 
                      longitude, "&lat=", latitude)
        
        # GET content from the Photon API
        data <- content(GET(url), encoding="UTF-8")
        
        # Sometimes data$features has nothing in it, so first check if it has something
        if(length(data$features) != 0) {
          PhotonInfo <- data$features[[1]]$properties
          
          # Different information is passed back sometimes, so try to find the best options
          # First, check if a country is listed and put it in the appropriate column
          if(length(PhotonInfo$country) != 0) {
            occs[i,4] <- as.character(PhotonInfo$country)
          }
          
          # Next, try to fill in the island column
          # The "locality" part of the Photon data often has the island name
          # If that part is empty, get the "county" part instead - COUNTY COULD BE A PROBLEM
          if(length(PhotonInfo$locality) != 0) {
            occs[i,5] <- as.character(PhotonInfo$locality)
          } else if(length(PhotonInfo$county) != 0) {
            occs[i,5] <- as.character(PhotonInfo$county)
          }
          
          
        }
        
        
      }
  
    }

  } # End fillgaps
  
  return(occs)
}


#' Find areas of land masses.
#' 
#' Reference a dataset of island names and areas to find the areas of the land masses relevant to the taxon of interest.
#' @param occs The dataframe that is returned by SSARP::findLand. If using a custom dataframe, ensure that it has the following columns in order: "Species", "Longitude", "Latitude", "First", "Second", "Third"
#' @return A dataframe of the species name, island name, and island area
#' @examples 
#' \dontrun{areas <- findAreas(occs)}
#' @export

findAreas <- function(occs){
  # Remove rows where First, Second, and Third are all NA
  # Create vector to hold row numbers
  minus <- rep(NA, nrow(occs))
  # Loop through dataframe
  for(i in c(1:nrow(occs))){
    if(is.na(occs[i,6]) && is.na(occs[i,5]) && is.na(occs[i,4])) {
      minus[i] <- i
    }
  }
  # Remove NAs from row number vector
  minus <- minus[!is.na(minus)]
  
  occs <- occs[-minus,]
  
  # Rename rows to make future loops make more sense
  #num_rows <- nrow(occs)
  #rownames(occs) <- c(1:num_rows)
  
  # Add a temporary key-value pair to initialize
  IslandDict <- Dict$new(
    bloop = 108
  )
  
  # For each island name in the current dataframe, find the area and add the pair to the dictionary
  
  # First, create an empty list of island names
  islands <- list()
  
  # Next, go through the occs dataframe and see if the Third column has a name in it
  # If yes, add to the island list. If NA, go to the Second column. If NA, go to the First column
  
  for(i in c(1:nrow(occs))) {
    if(!is.na(occs[i,6])) {
      islands[i] <- occs[i,6]
    }
    else if (!is.na(occs[i,5])){
      islands[i] <- occs[i,5]
    }
    else if (!is.na(occs[i,4])){
      islands[i] <- occs[i,4]
    }
  }
  
  # Next, eliminate duplicate entries in the list
  uniq_islands <- unique(islands)
  
  # Next, add the island names as keys and their corresponding areas as values
  # Get island areas from built-in area file
  area_file <- SSARP::island_areas
  #area_file <- read.csv("island_areas.csv")
  
  # Look through the island area file and find the names in the uniq_islands list
  for (i in c(1:length(uniq_islands))) {
    print(i)
    
    for(j in c(1:nrow(area_file))) {
      
      area_compare <- area_file[j,5]
      area_compare2 <- paste0(area_file[j,5], " Island")
      uniq_compare <- as.character(uniq_islands[i])
      uniq_compare2 <- paste0(as.character(uniq_islands[i]), " Island")
      
      if(is.na(uniq_compare)) {
        break
      }
      
      if(area_compare == uniq_compare || area_compare2 == uniq_compare || area_compare == uniq_compare2 || area_compare2 == uniq_compare2) {
        #testing[islands[i]] <- areas[i]
        #print(as.character(uniq_islands[i]))
        #print(area_file[j,3])
        IslandDict[as.character(uniq_islands[i])] <- area_file[j,3]
        #print("Found the island name for: ")
        #print(i)
        break # Break the inner loop when you find the island name
        
      }
      
      #else if(agrep(area_compare, uniq_compare, max.distance=7) == 1) {
      #  testing[as.character(uniq_islands[i])] <- area_file[j,4]
      #  print("agrep found the island name for: ")
      #  print(i)
      #  break
      #}
      
    }
  }
  
  # Use the dictionary to add the areas to the final dataframe
  areas <- rep(0, times = nrow(occs))
  
  for(i in c(1:nrow(occs))) {
    
    if(!is.na(occs[i,6]) && IslandDict$has(occs[i,6])){
      print(i)
      areas[i]<-IslandDict$get(occs[i,6])
    }
    else if(!is.na(occs[i,5]) && IslandDict$has(occs[i,5])){
      print(i)
      areas[i]<-IslandDict$get(occs[i,5])
    }
    else if(!is.na(occs[i,4]) && IslandDict$has(occs[i,4])){
      print(i)
      areas[i]<-IslandDict$get(occs[i,4])
    }
    else {
      areas[i]<-NA
    }

  }
  
  # Create final dataframe
  occs_final <- cbind(occs, areas)
  
  return(occs_final)
  
}

#' Remove continents from area dataframe.
#' 
#' Reference a list of continental areas to remove them from the dataframe output by SSARP::findAreas().
#' @param occs The dataframe that is returned by SSARP::findAreas. I do not recommend using a custom dataframe for this function because it references areas given by the area database used in SSARP::findAreas().
#' @return A dataframe of the species name, island name, and island area (without continents)
#' @examples 
#' \dontrun{new_areas <- removeContinents(occs)}
#' @export

removeContinents <- function(occs){
  # List of continental areas that could be added through the use of findAreas()
  continents <- c(5.50e13, 3.04e13, 1.78e13, 2.47e13)
  for(i in 1:length(continents)){
    exclude <- which(occs$areas == continents[i])
    occs <- occs[-exclude,]
  }
  
  return(occs)
}
