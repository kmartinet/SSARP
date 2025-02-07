#' Create a species-area relationship plot (SARP)
#' 
#' Use segmented regression to create a species-area relationship plot (SARP). The X axis represents log(island area) and the Y axis represents log(number of species)
#' @param occurrences The dataframe output by findAreas (or if using a custom dataframe, ensure that it has the following columns: Species, areas)
#' @param npsi The maximum number of breakpoints to estimate for model selection.  Default: 2
#' @return A list of 3 including: the summary output, the segmented regression object, and the aggregated dataframe used to create the plot
#' @examples 
#' \dontrun{
#' seg <- SARP(occurrences)
#' summary <- seg[1]
#' }
#' @importFrom segmented segmented
#' @importFrom stats AIC
#' @importFrom checkmate assertDataFrame assertNumeric
#' @export

SARP <- function(occurrences, npsi = 2) {
  # Checkmate input validation
  assertDataFrame(occurrences)
  assertNumeric(npsi)
  
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
  
  # First, create all requested models and compare their AIC scores to determine the best-fit
  # Empty list to populate with AIC scores
  aic_scores <- list()
  # Linear model is already created above
  aic_scores[1] <- AIC(linear)
  # Name the list element in case the user wants to examine the AIC scores
  names(aic_scores)[1] <- "Linear"
  
  # Create a segmented model for each level of npsi specified by the user
  # This only makes sense if the user does not specify that they want zero breakpoints
  if(npsi != 0){
    for(i in c(1:npsi)){
      seg <- segmented(linear, seg.Z = ~x, npsi = i, control = seg.control(display = FALSE, quant = TRUE))
      # Since the first index is always linear, use i+1
      aic_scores[i+1] <- AIC(seg)
      # Name the list element
      names(aic_scores)[i+1] <- paste0("BP", i)
    }
  }
  
  # Find the smallest AIC score (the best-fit model)
  min_score <- which.min(aic_scores)
  
  # If the min_score is index 1, then the best-fit model is linear
  if(min_score == 1){
    plot(dat,
         xlim = c(x_min, (x_max + 0.5)),
         ylim = c(y_min, (y_max + 0.5)),
         ylab = "Log Number of Species",
         xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
         main = "Species-Area Relationship",
         pch = 16)
    abline(linear)
    
    summary_line <- summary(linear)
    
    result <- list("summary" = summary_line, "linObj" = linear, "aggDF" = dat, "AICscores" = aic_scores)
    
    class(result) <- "SAR"
    
    return(result)
  } else if(min_score == 2){
    # Linear is the object we're segmenting, seg.Z is the continuous variable, npsi is the number of breakpoints to estimate, 
    # control is the bootstrap parameters (display = FALSE stops it from printing each iteration)
    # If min_score is 2, the best-fit model has one breakpoint
    seg <- segmented(linear, seg.Z = ~x, npsi = 1, control = seg.control(display = FALSE))
    
    # Plot the breakpoint regression line
    plot(seg, rug = FALSE,
         xlim = c(x_min, (x_max + 0.5)),
         ylim = c(y_min, (y_max + 0.5)),
         ylab = "Log Number of Species",
         xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
         main = "Species-Area Relationship")
    # Add the points
    points(dat$x, dat$y, pch = 19)
    
    # Save the summary as an object to add to the result list
    summary_seg <- summary(seg)
    
    result <- list("summary" = summary_seg, "segObj" = seg, "aggDF" = dat, "AICscores" = aic_scores)
    
    class(result) <- "SAR"
    
    return(result)
  } else{
    # If min_score > 2, then the best-fit model has multiple breakpoints
    # If npsi is > 1, the seg object plots differently, so it needs a separate else statement
    # The actual number of segments is the index of the minimum AIC minus 1 (because the first index is always linear)
    n_psi <- min_score - 1
    seg <- segmented(linear, seg.Z = ~x, npsi = n_psi, control = seg.control(display = FALSE))
    
    # Plot defaults to multiple outputs when npsi > 1, so my npsi = 1 plot doesn't apply
    plot(seg)
    
    # Save the summary as an object to add to the result list
    summary_seg <- summary(seg)
    
    result <- list("summary" = summary_seg, "segObj" = seg, "aggDF" = dat, "AICscores" = aic_scores)
    
    class(result) <- "SAR"
    
    return(result)
  }
}
