#' Create a presence-absence dataframe for a given occurrence record dataframe
#' 
#' Use a dataframe output by SSARP::findAreas to determine which species occur on specific islands by creating a presence-absence dataframe. A 1 represents presence and a 0 represents absence.
#' @param occs The dataframe output by SSARP::findAreas, or if using a custom dataframe, ensure that it has the following named columns:
#' - "areas" containing the areas associated with the land masses of interest
#' - "Species" containing the names of the species living on those islands
#' - A 6th column containing locality information. In the SSARP workflow, this column is called "First" and contains the country name
#' - A 7th column containing locality information. In the SSARP workflow, this column is called "Second" and contains a province or island name
#' - An 8th column containing locality information. In the SSARP workflow, this column is called "Third" and contains the island name if the 7th column does not contain the island name
#' @return A dataframe with a row for each island in the given occurrence record dataframe and a column for each species. Within each species column, a 1 represents the presence of that species on the island corresponding to the given row, and a 0 represents the absence of that species on the island corresponding to the given row.
#' @examples 
#' \dontrun{
#' key <- getKey(query = "Anolis", rank = "genus")
#' dat <- getData(key = key, limit = 100)
#' land <- findLand(occurrences = dat)
#' areas <- findAreas(occs = land)
#' pres_abs <- getPresenceAbsence(areas)
#' }
#' @importFrom reshape2 dcast
#' @export

getPresenceAbsence <- function(occs) {
  # Create a dataframe that counts how many records there are for each species in each island
  new_occs <- dcast(occs, areas ~ Species, fun.aggregate = length, value.var = "Species")
  
  # We want those counts to be 1 if the species is there and 0 if it is not, so
  #  we need to replace the numbers greater than 1 with a 1
  # Before we do that, save the island area column (because they're all > 1)
  island_areas <- new_occs[,1]
  
  # Now replace anything that isn't a zero with a 1
  new_occs[new_occs != 0] <- 1
  
  # Add the island area column back
  new_occs[,1] <- island_areas
  
  # We probably want the name of the islands on here too, right?
  # Create a new dataframe of just names to splice on the new_occs dataframe
  area_names <- data.frame()
  for(i in c(1:length(new_occs$areas))){
    
    # Pick the first occurrence of the ith area to get the name
    area_names <- rbind(area_names, occs[which(occs$areas == new_occs[i,1])[1], c(6:8)])
    
  }
  
  # Now cbind area_names to new_occs
  final_occs <- cbind(area_names, new_occs)
  
  # Rownames won't make sense, so reset them to 1 to n
  rownames(final_occs) <- c(1:length(final_occs$First))
  
  return(final_occs)
  
}