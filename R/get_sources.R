#' Gather sources from GBIF data for citation
#' 
#' When using data obtained via `SSARP::get_data()` and filtered with 
#' `SSARP::find_areas()` for a publication, you must keep a record of the  
#' datasets used in your analysis. This function assists in creating the  
#' dataframe necessary to follow GBIF's citation guidelines (see References).
#' @param occs The occurrence record dataframe returned by `SSARP::get_data()` 
#' or `SSARP::find_areas()`.
#' @return A dataframe of dataset keys and the number of occurrence records 
#' associated with each key that were gathered with `SSARP::get_data()` and/or 
#' filtered with `SSARP::find_areas()`.
#' @references 
#' - [GBIF citation guidelines](https://www.gbif.org/citation-guidelines)
#' - Data obtained via `SSARP::get_data()` and filtered with 
#' `SSARP::find_areas()` falls under [the derived datasets distinction](https://www.gbif.org/derived-dataset/about)
#' - [More information about creating derived datasets](https://data-blog.gbif.org/post/derived-datasets/)
#' @examples 
#' # The GBIF key for the Anolis genus is 8782549
#' #  Obtained with: key <- get_key(query = "Anolis", rank = "genus")
#' key <- 8782549
#' # Read in example dataset obtained through:
#' #  dat <- get_data(key = key, limit = 100)
#' dat <- read.csv(system.file("extdata",
#'                             "SSARP_Example_Dat.csv",
#'                             package = "SSARP"))
#' source_df <- get_sources(occs = dat)
#' 
#' @export

get_sources <- function(occs){
  # checkmate input validation
  checkmate::assertDataFrame(occs)
  
  # Make sure that datasetKey is a column before proceeding
  if("datasetKey" %in% colnames(occs)) {
    # Get datasetKey column
    key_col <- as.data.frame(occs$datasetKey)
    
    # Filter 
    count_df <- key_col |> dplyr::count(occs$datasetKey)
    # Name columns
    colnames(count_df) <- c("datasetKey", "n")
    
    return(count_df)
  } else {
    cli::cli_alert_warning("datasetKey column not found.")
    return(NULL)
  }
} 
