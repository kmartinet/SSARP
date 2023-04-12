#' Create a species-area relationship plot (SARP)
#' 
#' Use segmented regression to create a species-area relationship plot (SARP). The X axis represents log(island area) and the Y axis represents log(number of species)
#' @param occurrences The dataframe output by findAreas (or if using a custom dataframe, ensure that it has the following columns: Species, areas)
#' @param npsi The number of breakpoints to estimate. Default: 1
#' @return A list of 3 including: the summary output, the segmented regression object, and the aggregated dataframe used to create the plot
#' @examples 
#' \dontrun{
#' seg <- SARP(occurrences)
#' summary <- seg[1]
#' }
#' @import tidyverse
#' @import segmented
#' @export

SARP <- function(occurrences, npsi = 1) {
  # Aggregate function: 
  #   data is the dataframe you want the info to come from
  #   formula Species ~ Area means to group scientific names by area
  #   function(x) length(unique(x)) tells it to give me the number of unique species for each unique island area
  agg <- aggregate(data = occurrences, Species ~ areas, function(x) length(unique(x)))
  
  # Segmented package prefers tidy dataframes, so make one for it
  dat <- data.frame(x = log(agg$areas), y = log(agg$Species))
  
  # Run a linear model on the data to use in creating segmented/breakpoint regression
  linear <- lm(y ~ x, data = dat)
  # Linear is the object we're segmenting, seg.Z is the continuous variable, npsi is the number of breakpoints to estimate, 
  # control is the bootstrap parameters (display = FALSE stops it from printing each iteration)
  seg <- segmented(linear, seg.Z = ~x, npsi = npsi, control = seg.control(display = FALSE))
  
  # Plot the breakpoint regression line
  plot(seg, rug = FALSE,
       ylab = "Log Number of Species",
       xlab = "Log Island Area (m^2)",
       main = "Species-Area Relationship")
  # Add the points
  points(dat$x, dat$y, pch = 19)
  
  # Save the summary as an object to add to the result list
  summary_seg <- summary(seg)
  
  result <- list("summary" = summary_seg, "segObj" = seg, "aggDF" = dat)
  
  return(result)
}