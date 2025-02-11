#' Gather sources from GBIF data for citation
#' 
#' When using data obtained via SSARP::getData for a publication, you must keep a record of the datasets used in your analysis. This function assists in creating the dataframe necessary to follow GBIF's citation guidelines (see References).
#' @param occs The occurrence record dataframe returned by SSARP::getData.
#' @return A dataframe of dataset keys and the number of occurrence records associated with each key that were gathered with SSARP::getData.
#' @references 
#' - [GBIF citation guidelines](https://www.gbif.org/citation-guidelines)
#' - Data obtained via SSARP::getData falls under [the derived datasets distinction](https://www.gbif.org/derived-dataset/about)
#' - [More information about creating derived datasets](https://data-blog.gbif.org/post/derived-datasets/)
#' @examples 
#' \dontrun{
#' occs <- getData(key = key, limit = 10000)
#' source_df <- getSources(occs = occs)
#' }
#' @importFrom dplyr count
#' @importFrom checkmate assertDataFrame
#' @importFrom cli cli_alert_warning
#' @export

getSources <- function(occs){
  # checkmate input validation
  assertDataFrame(occs)
  
  # Make sure that datasetKey is a column before proceeding
  if("datasetKey" %in% colnames(occs)) {
    # Get datasetKey column
    key_col <- as.data.frame(occs$datasetKey)
    
    # Filter 
    count_df <- key_col |> count(occs$datasetKey)
    # Name columns
    colnames(count_df) <- c("datasetKey", "n")
    
    return(count_df)
  } else {
    cli_alert_warning("datasetKey column not found.")
    return(NULL)
  }
} 
