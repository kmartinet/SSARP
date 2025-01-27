#' Get tip speciation rates from BAMM (Rabosky 2014) analysis
#' 
#' Use the BAMMtools package (Rabosky et al. 2014) to extract tip speciation rates from user-supplied BAMM analysis objects.
#' @param label_type Either "epithet" or "binomial" (default): describes the type of tip label in the tree used for the BAMM analysis. If "epithet," only the species epithet will be used to match speciation rates to tips in the returned occurrence dataframe. If "binomial," the full species name (including genus) will be used to match speciation rates to tips in the returned occurrence dataframe.
#' @param occurrences The occurrence record dataframe output from the SSARP pipeline. If you would like to use a custom dataframe, please make sure that there are columns titled "Genus" and "Species"
#' @param edata The eventdata object created by using the getEventData() function in BAMMtools
#' @return A dataframe that includes speciation rates for each species in the occurrence record dataframe
#' @examples 
#' \dontrun{
#' occ_speciation <- speciationBAMM("epithet", occs, edata)
#' }
#' @import tidyverse
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom checkmate assertString assertDataFrame
#' @export

speciationBAMM <- function(label_type = "binomial", occurrences, edata) {
  # Checkmate input validation
  assertString(label_type)
  assertDataFrame(occurrences)
  
  # Create a named number vector of speciation rates
  speciation_rates <- edata$meanTipLambda
  names(speciation_rates) <- edata$tip.label
  
  # For each Species name in speciation_rates, look for that name in the Species column of the occurrence record dataframe and add the appropriate rate
  
  # Initialize a rate column with NAs
  occurrences <- occurrences %>%
    mutate(rate = NA)
  
  # If the user specified label_type = "epithet"
  if(label_type == "epithet"){
    # For each name in speciation_rates, look for that name in the Species column of the occurrence record dataframe and add the appropriate rate
    for(i in names(speciation_rates)){
      occurrences <- occurrences %>%
        mutate(rate = case_when(
          grepl(i, Species) ~ speciation_rates[[i]],
          TRUE ~ rate # Keeps values for non-matching rows
        ))
    }
  } else if(label_type == "binomial"){
    # The occurrence record dataframe has separate "Genus" and "Species" columns, so they should be combined for this label type
    occurrences$Binomial <- paste(occurrences$Genus, occurrences$Species, sep = " ")
    # Now, for each name in speciation_rates, look for that name in the Binomial column of the occurrence record dataframe and add the appropriate rate
    for(i in names(speciation_rates)){
      occurrences <- occurrences %>%
        mutate(rate = case_when(
          grepl(i, Binomial) ~ speciation_rates[[i]],
          TRUE ~ rate # Keeps values for non-matching rows
        ))
    }
  }

  # Remove rows with NA in rate column
  occurrences <- occurrences[!is.na(occurrences$rate),]
  
  return(occurrences)
}