#' Get speciation rates using the DR statistic (Jetz et al. 2012)
#' 
#' Use the DRstat method from the epm package (Title et al. 2022) and output a dataframe for use in SSARP's speciation-area relationship pipeline. This method also removes any species rows without rates (this is most likely to occur when the tree does not have all of the species included in the occurrence record dataframe)
#' @param tree The dated phylogenetic tree that corresponds with the taxa to be included in a speciation-area relationship
#' @param occurrences The occurrence record dataframe output from the SSARP pipeline. If you would like to use a custom dataframe, please make sure that there is a column titled "Species"
#' @return A dataframe that includes speciation rates for each species in the occurrence record dataframe
#' @examples 
#' \dontrun{
#' occ_speciation <- speciationDR(tree, occs)
#' }
#' @import tidyverse
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @import epm
#' @import ape
#' @export

speciationDR <- function(tree, occurrences) {
  speciation_rates <- DRstat(tree)
  
  # Initialize a rate column with NAs
  sp_occ <- occurrences %>%
    mutate(rate = NA)
  
  # For each name in speciation_rates, look for that name in the occurrence record dataframe and add the appropriate rate
  for(i in names(speciation_rates)){
    sp_occ <- sp_occ %>%
      mutate(rate = case_when(
        grepl(i, Species) ~ speciation_rates[[i]],
        TRUE ~ rate # Keeps values for non-matching rows
      ))
  }
  
  # Remove rows with NA in rate column
  sp_occ <- sp_occ[!is.na(sp_occ$rate),]
  
  return(sp_occ)
  
}
