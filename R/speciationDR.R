#' Get speciation rates using the DR statistic (Jetz et al. 2012)
#' 
#' Use methodology from Sun and Folk (2020) to calculate the DR statistic for each tip of a given tree and output a dataframe for use in SSARP's speciation-area relationship pipeline. This method also removes any species rows without rates (this is most likely to occur when the tree does not have all of the species included in the occurrence record dataframe)
#' @param tree The dated phylogenetic tree that corresponds with the taxa to be included in a speciation-area relationship
#' @param label_type Either "epithet" or "binomial" (default): describes the type of tip label in the provided tree. If "epithet," only the species epithet will be used to match speciation rates to tips in the returned occurrence dataframe. If "binomial," the full species name (including genus) will be used to match speciation rates to tips in the returned occurrence dataframe. 
#' @param occurrences The occurrence record dataframe output from the SSARP pipeline. If you would like to use a custom dataframe, please make sure that there are columns titled "Genus" and "Species"
#' @return A dataframe that includes speciation rates for each species in the occurrence record dataframe
#' @references 
#' - Jetz, W., Thomas, G.H, Joy, J.B., Harmann, K., & Mooers, A.O. (2012). The global diversity of birds in space and time. Nature, 491: 444-448.
#' - Sun, M. & Folk, R.A. (2020). Cactusolo/rosid_NCOMMS-19-37964-T: Code and data for rosid_NCOMMS-19-37964 (Version V.1.0). Zenodo. http://doi.org/10.5281/zenodo.3843441
#' @examples 
#' \dontrun{
#' key <- getKey(query = "Anolis", rank = "genus")
#' dat <- getData(key = key, limit = 100)
#' land <- findLand(occurrences = dat)
#' areas <- findAreas(occs = land)
#' 
#' # Assuming that the user has a tree file called "anolis.tree"
#' tree <- ape::read.tree("anolis.tree")
#' 
#' occ_speciation <- speciationDR(tree = tree, label_type = "epithet", occurrences = areas)
#' }
#' @importFrom dplyr mutate case_when
#' @importFrom checkmate assertString assertDataFrame
#' @export

speciationDR <- function(tree, label_type = "binomial", occurrences) {
  # Checkmate input validation
  assertString(label_type)
  assertDataFrame(occurrences)
  
  # Use DR statistic methodology from Sun and Folk (2020) to obtain per-tip DR stats
  # Adapted from the "DR_statistic.R" file (see References section in the speciationDR documentation)
  rootnode <- length(tree$tip.label) + 1
  speciation_rates <- numeric(length(tree$tip.label))
  for (i in 1:length(speciation_rates)){
    node <- i
    index <- 1
    qx <- 0
    while (node != rootnode){
      el <- tree$edge.length[tree$edge[,2] == node]
      node <- tree$edge[,1][tree$edge[,2] == node]            
      qx <- qx + el* (1 / 2^(index-1))            
      index <- index + 1
    }
    speciation_rates[i] <- 1/qx
  }
  names(speciation_rates) <- tree$tip.label
  
  # Initialize a rate column with NAs
  sp_occ <- occurrences |> mutate(rate = NA)
  
  # If the user specified label_type = "epithet"
  if(label_type == "epithet"){
    # For each name in speciation_rates, look for that name in the Species column of the occurrence record dataframe and add the appropriate rate
    for(i in names(speciation_rates)){
      sp_occ <- sp_occ |>
        mutate(rate = case_when(
          grepl(i, Species) ~ speciation_rates[[i]],
          TRUE ~ rate # Keeps values for non-matching rows
        ))
    }
  } else if(label_type == "binomial"){
    # The occurrence record dataframe has separate "genus" and "species" columns, so they should be combined for this label type
    sp_occ$Binomial <- paste(sp_occ$Genus, sp_occ$Species, sep = " ")
    # Now, for each name in speciation_rates, look for that name in the Binomial column of the occurrence record dataframe and add the appropriate rate
    for(i in names(speciation_rates)){
      sp_occ <- sp_occ |>
        mutate(rate = case_when(
          grepl(i, Binomial) ~ speciation_rates[[i]],
          TRUE ~ rate # Keeps values for non-matching rows
        ))
    }
  }
  
  # Remove rows with NA in rate column
  sp_occ <- sp_occ[!is.na(sp_occ$rate),]
  
  return(sp_occ)
  
}
