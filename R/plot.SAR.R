#' Plot a species-area relationship
#' 
#' Function for plotting species-area relationship objects from the SSARP::SARP() function
#' @param x The SAR object that will be plotted
#' @param ... Functions to pass to plot()
#' @return A plot of your species-area relationship
#' @examples 
#' \dontrun{
#' seg <- SARP(occurrences)
#' plot(seg)
#' }
#' @import tidyverse
#' @import segmented
#' @export

plot.SAR <- function(x, ...){
  
  # Record x and y min/max for plot limits
  dat <- x[["aggDF"]]
  
  x_max <- max(dat$x)
  x_min <- min(dat$x)
  y_max <- max(dat$y)
  y_min <- min(dat$y)
  
  # Segmented plotting
  if(!is.null(x[["segObj"]])){ 
    plot(x[["segObj"]], rug = FALSE,
         xlim = c(x_min, (x_max + 0.5)),
         ylim = c(y_min, (y_max + 0.5)),
         ylab = "Log Number of Species",
         xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
         main = "Species-Area Relationship")
    # Add the points
    points(x[["aggDF"]]$x, x[["aggDF"]]$y, pch = 19)
  } 
  # Line plotting
  else if(!is.null(x[["linObj"]])){
    plot(x[["aggDF"]],
         xlim = c(x_min, (x_max + 0.5)),
         ylim = c(y_min, (y_max + 0.5)),
         ylab = "Log Number of Species",
         xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
         main = "Species-Area Relationship",
         pch = 16)
    abline(x[["linObj"]])
  }
}
