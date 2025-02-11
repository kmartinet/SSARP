#' Remove continents from area dataframe.
#' 
#' Reference a list of continental areas to remove them from the dataframe output by SSARP::findAreas().
#' @param occs The dataframe that is returned by SSARP::findAreas. I do not recommend using a custom dataframe for this function because it references areas given by the area database used in SSARP::findAreas(). If you must use a custom dataframe, please ensure  that the landmass areas are in the 9th column of the dataframe.
#' @return A dataframe of the species name, island name, and island area (without continents)
#' @examples 
#' \dontrun{new_areas <- removeContinents(occs)}
#' @importFrom checkmate assertDataFrame
#' @export

removeContinents <- function(occs){
  # Checkmate input validation
  assertDataFrame(occs)
  
  # List of continental areas that could be added through the use of findAreas()
  continents <- c(5.50e13, 3.04e13, 1.78e13, 2.47e13)
  for(i in seq(continents)){
    exclude <- which(occs[,9] == continents[i])
    if(length(exclude) != 0){
      occs <- occs[-exclude,]
    }
  }
  
  return(occs)
}
