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
  #   formula Species ~ Area means to group scientific names by area
  #   function(x) length(unique(x)) tells it to give me the number of unique species for each unique island area
  agg <- aggregate(data = occurrences, Species ~ areas, function(x) length(unique(x)))
  
  # Segmented package prefers tidy dataframes, so make one for it
  dat <- data.frame(x = log(agg$areas), y = log(agg$Species))
  
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
         xlim = c(x_min, (x_max+0.5)),
         ylim = c(y_min, (y_max+0.5)),
         ylab = "Log Number of Species",
         xlab = "Log Island Area (m^2)",
         main = "Species-Area Relationship",
         pch = 16)
    abline(linear)
    
    summary_line <- summary(linear)
    
    result <- list("summary" = summary_line, "linObj" = linear, "aggDF" = dat)
    
    class(result) <- "SAR"
    
    return(result)
  }
  else if(npsi == 1){
    # Linear is the object we're segmenting, seg.Z is the continuous variable, npsi is the number of breakpoints to estimate, 
    # control is the bootstrap parameters (display = FALSE stops it from printing each iteration)
    seg <- segmented(linear, seg.Z = ~x, npsi = npsi, control = seg.control(display = FALSE))
    
    # Plot the breakpoint regression line
    plot(seg, rug = FALSE,
         xlim = c(x_min, (x_max+0.5)),
         ylim = c(y_min, (y_max+0.5)),
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
  }
  else{
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

#' Quickly create a species-area relationship
#' 
#' Use the SSARP workflow for creating a species-area relationship for island species using occurrence data from GBIF without having to go through every individual step.
#' @param taxon The name of the taxon of interest.
#' @param rank The taxonomic rank associated with the taxon of interest.
#' @param limit The number of GBIF occurrence records to obtain.
#' @param continent (logical) If TRUE, continental areas are removed from the species-area relationship data. If FALSE, continental areas are not removed from the species-area relationship data. Default: TRUE
#' @param npsi The number of breakpoints to estimate. Default: 1
#' @return A list of 3 including: the summary output, the segmented regression object, and the aggregated dataframe used to create the plot.
#' @examples 
#' \dontrun{
#' seg <- quickSARP("Phelsuma", "genus", 1000, 1)
#' }
#' @import tidyverse
#' @import segmented
#' @export

quickSARP <- function(taxon, rank, limit = 100, continent = TRUE, npsi = 1) {
  # Get the taxon key from GBIF
  print("Finding taxon key")
  key <- getKey(taxon, rank)
  
  print("Gathering data")
  # Use the key to get data
  dat <- getData(key, limit = limit)
  
  print("Finding land")
  # Find the name of the land on which the occurrence points were found
  land <- findLand(dat)
  
  print("Gathering areas")
  # Find the areas of the land
  areas <- findAreas(land)

  if(continent == TRUE) {
  print("Removing continental areas")
    # Remove continents from the dataframe
    areas <- removeContinents(areas)
  }
  
  # Create the species-area relationship plot
  SAR_obj <- SARP(areas, npsi)
  
  return(SAR_obj)
  
}



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
         xlim = c(x_min, (x_max+0.5)),
         ylim = c(y_min, (y_max+0.5)),
         ylab = "Log Number of Species",
         xlab = "Log Island Area (m^2)",
         main = "Species-Area Relationship")
    # Add the points
    points(x[["aggDF"]]$x, x[["aggDF"]]$y, pch = 19)
  } 
  # Line plotting
  else if(!is.null(x[["linObj"]])){
    plot(x[["aggDF"]],
         xlim = c(x_min, (x_max+0.5)),
         ylim = c(y_min, (y_max+0.5)),
         ylab = "Log Number of Species",
         xlab = "Log Island Area (m^2)",
         main = "Species-Area Relationship",
         pch = 16)
    abline(x[["linObj"]])
  }
  
}

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
#' @import tidyverse
#' @import segmented
#' @export

print.SAR <- function(x, printlen = NULL, ...){
  print(x$summary)
}

