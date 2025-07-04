#' Remove continents from area dataframe.
#' 
#' Reference a list of continental areas to remove them from the dataframe 
#' output by `SSARP::find_areas()`.
#' @param occs The dataframe that is returned by `SSARP::find_areas()`. I do not 
#' recommend using a custom dataframe for this function because it references 
#' areas given by the area database used in `SSARP::find_areas()`. If you must 
#' use a custom dataframe, please ensure  that the landmass areas are in the 9th 
#' column of the dataframe.
#' @return A dataframe of the species name, island name, and island area 
#' (without continents)
#' @examples 
#' # The GBIF key for the Anolis genus is 8782549
#' #  Obtained with: key <- get_key(query = "Anolis", rank = "genus")
#' key <- 8782549
#' # Read in example dataset obtained through:
#' #  dat <- get_data(key = key, limit = 100)
#' dat <- read.csv(system.file("extdata",
#'                             "SSARP_Example_Dat.csv",
#'                             package = "SSARP"))
#' occs <- find_land(occurrences = dat)
#' areas <- find_areas(occs = occs)
#' new_areas <- remove_continents(areas)
#' 
#' @export

remove_continents <- function(occs){
  # Checkmate input validation
  checkmate::assertDataFrame(occs)
  
  # List of continental areas that could be added through the use of 
  #  SSARP::find_areas()
  continents <- c(5.50e13, 3.04e13, 1.78e13, 2.47e13)
  for(i in seq(continents)){
    exclude <- which(occs[,10] == continents[i])
    if(length(exclude) != 0){
      occs <- occs[-exclude,]
    }
  }
  
  return(occs)
}
