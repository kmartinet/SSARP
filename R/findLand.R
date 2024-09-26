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
  # First, use worldHires (world2Hires has a different coordinate system)
  where <- maps::map.where(database="mapdata::worldHires", x=lon, y=lat)
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
  
  occs <- as.data.frame(cbind(occurrences$acceptedScientificName, occurrences$genericName, occurrences$specificEpithet, lon, lat, where2))
  # Separate the where column into two separate columns - Country and Island
  # But sometimes there are three...
  suppressWarnings(occs <- occs %>% tidyr::separate(where2, c("First", "Second", "Third"), sep = ":"))
  colnames(occs) <- c("SpeciesName", "Genus", "Species", "Longitude", "Latitude", "First", "Second", "Third")
  
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
