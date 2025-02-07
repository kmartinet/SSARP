#' Print species-area relationship summary
#' 
#' Function for printing the summary of species-area relationship objects from the SSARP::SARP() function
#' @param x The SAR object of interest
#' @param ... Parameters to pass to print()
#' @param printlen Should always be NULL
#' @return The summary of your species-area relationship
#' @examples 
#' \dontrun{
#' seg <- SARP(occurrences)
#' print(seg)
#' }
#' @export

print.SAR <- function(x, printlen = NULL, ...){
  print(x$summary)
}
