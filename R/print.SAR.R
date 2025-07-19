#' Print species-area relationship summary
#'
#' Function for printing the summary of species-area relationship objects from
#' the `ssarp::create_SAR()` function
#' @param x The SAR object of interest
#' @param ... Parameters to pass to print()
#' @param printlen Should always be NULL
#' @return The summary of your species-area relationship
#' @examples
#' # The GBIF key for the Anolis genus is 8782549
#' # Read in example dataset filtered from:
#' #  dat <- rgbif::occ_search(taxonKey = 8782549, 
#' #                           hasCoordinate = TRUE,
#' #                           limit = 10000)
#' dat <- read.csv(system.file("extdata",
#'                             "ssarp_Example_Dat.csv",
#'                             package = "ssarp"))
#' occs <- find_land(occurrences = dat)
#' areas <- find_areas(occs = occs)
#' seg <- create_SAR(areas, npsi = 0)
#' print(seg)
#'
#' @export

print.SAR <- function(x, printlen = NULL, ...) {
  print(x$summary)
}
