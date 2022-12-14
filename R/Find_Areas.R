#' Find the name of the land on which the occurrence points were found
#' 
#' Use various mapping tools to attempt to find the names of land masses where the occurrence points were found.
#' @param occurrences The dataframe output by getData (or if using a custom dataframe, ensure that it has the following columns: decimalLongitude, decimalLatitude, acceptedScientificName)
#' @param fillgaps (logical) Attempt to use Photon API to fill in gaps left by mapdata::map.where (TRUE) or only mapdata::map.where results (FALSE, default). While it is powerful, the Photon API does not have a standard location for island names in its returned information, so using it will likely require the returned dataframe to be cleaned by the user.
#' @return A dataframe of the species name, longitude, latitude, country, and island of occurrence
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
  # Use map.where to figure out what land mass the 
  #where<-maps::map.where(database="world2Hires", x=lon, y=lat)
  where<-maps::map.where(database="world", x=lon, y=lat)
  
  occs <- as.data.frame(cbind(occurrences$acceptedScientificName, lon, lat, where))
  # Separate the where column into two separate columns - Country and Island
  occs <- occs %>% tidyr::separate(where, c("Country", "Island"), sep = ":")
  colnames(occs) <- c("Species", "Longitude", "Latitude", "Country", "Island")
  
  if(fillgaps == TRUE) {
    # There might still be a lot of NA entries, so use Photon to try to fill in gaps

    for(i in c(1:nrow(occs))) {
      if(is.na(occs[i,4])){
        # Get lon and lat
        longitude = occs[i,2]
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
          # If that part is empty, get the "county" part instead
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


#' Find areas of land masses. CURRENTLY BROKEN?
#' 
#' Reference a dataset of island names and areas to find the areas of the land masses relevant to the taxon of interest.
#' @param occs The dataframe that is returned by SSARP::findLand. If using a custom dataframe, ensure that it has an "Island" column and a "Country" column.
#' @return A dataframe of the species name, island name, and island area
#' @examples 
#' \dontrun{areas <- findAreas(occs)}
#' @export

findAreas <- function(occs){
  # Add a temporary key-value pair to initialize
  IslandDict <- Dict$new(
    bloop = 108
  )
  
  # For each island name in the current dataframe, find the area and add the pair to the dictionary
  
  # First, create an empty list of island names
  islands <- list()
  
  # Next, go through the occs dataframe and see if the island column has a name in it
  # If yes, add to the island list. If NA, go to the country column.
  
  for(i in c(1:nrow(occs))) {
    if(!is.na(occs[i,5])) {
      islands[i] <- occs[i,5]
    }
    else {
      islands[i] <- occs[i,4]
    }
  }
  
  # Next, eliminate duplicate entries in the list
  uniq_islands <- unique(islands)
  
  # Next, add the island names as keys and their corresponding areas as values
  # CAN'T GET DATA TO WORK??
  #area_file <- read.csv("data/EditedAllIslands.csv")
  area_file <- SSARP::island_areas # Should already be loaded with the package??
  
  # Look through the island area file and find the names in the uniq_islands list
  for (i in c(1:length(uniq_islands))) {
    print(i)
    
    for(j in c(1:nrow(area_file))) {
      
      area_compare <- area_file[j,5]
      uniq_compare <- as.character(uniq_islands[i])
      
      if(is.na(uniq_compare)) {
        break
      }
      
      if(area_compare == uniq_compare) {
        #testing[islands[i]] <- areas[i]
        testing[as.character(uniq_islands[i])] <- area_file[j,3]
        print("Found the island name for: ")
        print(i)
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

    if(is.na(occs[i,5])){
      
      if(!is.na(occs[i,4])) {
        print(i)
        areas[i]<-testing$get(occs[i,4])
      }
    }
    
    if(!is.na(occs[i,5])) {
      areas[i] <- testing$get(occs[i,5])
    }
  }
  
  # Create final dataframe
  occs_final <- cbind(occs, areas)
  
  return(occs_final)
  
}
