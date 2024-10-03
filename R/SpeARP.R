#' Create a speciation-area relationship plot
#' 
#' Use segmented regression to create a speciation-area relationship plot. The X axis represents log(island area) and the Y axis represents log(speciation rate)
#' @param occurrences The dataframe output by one of SSARP's speciation methods (speciationDR), or if using a custom dataframe, ensure that it has the following columns: Genus, Species, areas, rate
#' @param npsi The number of breakpoints to estimate. Default: 1
#' @return A list of 3 including: the summary output, the segmented regression object, and the aggregated dataframe used to create the plot
#' @examples 
#' \dontrun{
#' seg <- SpeARP(occurrences)
#' summary <- seg[1]
#' }
#' @import tidyverse
#' @import segmented
#' @export

SpeARP <- function(occurrences, npsi = 1) {
  # The purpose of this function is to create either a linear or segmented regression to visualize the relationship between speciation rate and island area
  #   formula rate ~ areas means to group scientific names by area
  #   function(x) length(unique(x)) tells it to give me the number of unique species for each unique island area
  agg <- aggregate(data = occurrences, rate ~ areas, function(x) mean(x))
  
  # Segmented package prefers tidy dataframes, so make one for it
  dat <- data.frame(x = log(agg$areas), y = log(agg$rate))
  
  # Run a linear model on the data to use in creating segmented/breakpoint regression
  linear <- lm(y ~ x, data = dat)
  
  # Record x and y min/max for plot limits
  x_max <- max(dat$x)
  x_min <- min(dat$x)
  y_max <- max(dat$y)
  y_min <- min(dat$y)
  
  # If the user does not want a breakpoint, they will input npsi = 0
  if(npsi == 0){
    plot(dat,
         xlim = c(x_min, (x_max + 0.5)),
         ylim = c(y_min, (y_max + 0.5)),
         ylab = "Log Number of Species",
         xlab = "Log Island Area (m^2)",
         main = "Species-Area Relationship",
         pch = 16)
    abline(linear)
    
    summary_line <- summary(linear)
    
    result <- list("summary" = summary_line, "linObj" = linear, "aggDF" = dat)
    
    class(result) <- "SAR"
    
    return(result)
  } else if(npsi == 1){
    # Linear is the object we're segmenting, seg.Z is the continuous variable, npsi is the number of breakpoints to estimate, 
    # control is the bootstrap parameters (display = FALSE stops it from printing each iteration)
    seg <- segmented(linear, seg.Z = ~x, npsi = npsi, control = seg.control(display = FALSE))
    
    # Plot the breakpoint regression line
    plot(seg, rug = FALSE,
         xlim = c(x_min, (x_max + 0.5)),
         ylim = c(y_min, (y_max + 0.5)),
         ylab = "Log Number of Species",
         xlab = "Log Island Area (m^2)",
         main = "Species-Area Relationship")
    # Add the points
    points(dat$x, dat$y, pch = 19)
    
    # Save the summary as an object to add to the result list
    summary_seg <- summary(seg)
    
    result <- list("summary" = summary_seg, "segObj" = seg, "aggDF" = dat)
    
    class(result) <- "SAR"
    
    return(result)
  } else{
    # If npsi is > 1, the seg object plots differently...
    seg <- segmented(linear, seg.Z = ~x, npsi = npsi, control = seg.control(display = FALSE))
    
    # Plot defaults to multiple outputs when npsi > 1, so my npsi = 1 plot doesn't apply
    plot(seg)
    
    # Save the summary as an object to add to the result list
    summary_seg <- summary(seg)
    
    result <- list("summary" = summary_seg, "segObj" = seg, "aggDF" = dat)
    
    class(result) <- "SAR"
    
    return(result)
  }
}
