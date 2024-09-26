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
    if(length(exclude) != 0){
      occs <- occs[-exclude,]
    }
  }
  
  return(occs)
}
