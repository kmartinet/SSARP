#' Create a speciation-area relationship plot
#' 
#' Use segmented regression to create a speciation-area relationship plot. The X axis represents log(island area) and the Y axis represents log(speciation rate)
#' @param occurrences The dataframe output by one of SSARP's speciation methods (speciationDR), or if using a custom dataframe, ensure that it has the following columns: areas, rate
#' @param npsi The maximum number of breakpoints to estimate for model selection.  Default: 1
#' @return A list of 3 including: the summary output, the segmented regression object, and the aggregated dataframe used to create the plot
#' @examples 
#' \dontrun{
#' key <- getKey(query = "Anolis", rank = "genus")
#' dat <- getData(key = key, limit = 100)
#' land <- findLand(occurrences = dat)
#' areas <- findAreas(occs = land)
#' occ_speciation <- speciationBAMM(label_type = "epithet", occurrences = areas, edata = edata)
#' seg <- SpeARP(occurrences = occ_speciation, npsi = 1, MS = FALSE)
#' summary <- seg[1]
#' }
#' @importFrom segmented segmented seg.control
#' @importFrom stats AIC aggregate lm
#' @importFrom checkmate assertDataFrame assertNumeric assertLogical
#' @importFrom graphics abline points
#' @export

SpeARP <- function(occurrences, npsi = 1) {
  # Checkmate input validation
  assertDataFrame(occurrences)
  assertNumeric(npsi)
  
  # The purpose of this function is to create either a linear or segmented regression to visualize the relationship between speciation rate and island area
  #   formula rate ~ areas means to group speciation rates by area
  #   function(x) mean(x, na.rm = TRUE) gives the mean of the rate for each area, while also removing any NA values
  agg <- aggregate(data = occurrences, rate ~ areas, function(x) mean(x, na.rm = TRUE))
  
  # Segmented package prefers tidy dataframes, so make one for it
  # If speciationMS was used to calculate speciation rates, make sure to not log-transform the rate column

  dat <- data.frame(x = log(agg$areas), y = log(agg$rate))
  
  # Run a linear model on the data to use in creating segmented/breakpoint regression
  linear <- lm(y ~ x, data = dat)
  
  # Record x and y min/max for plot limits
  x_max <- max(dat$x)
  x_min <- min(dat$x)
  y_max <- max(dat$y)
  y_min <- min(dat$y)
  
  # Create a buffer around y to slightly expand plot to ensure all points can be seen
  # Address buffer differently depending on the sign of y_max
  if(y_max < 0){
    # If y_max is negative, subtract
    y_buff <- y_max - (y_max*0.2)
  } else {
    y_buff <- y_max + (y_max*0.2)
  }
  
  # First, create all requested models and compare their AIC scores to determine the best-fit
  # Empty list to populate with AIC scores
  aic_scores <- list()
  # Linear model is already created above
  aic_scores[1] <- AIC(linear)
  
  # Create a segmented model for each level of npsi specified by the user
  # This only makes sense if the user does not specify that they want zero breakpoints
  if(npsi != 0){
    for(i in seq(npsi)){
      seg <- segmented(linear, seg.Z = ~x, npsi = i, control = seg.control(display = FALSE))
      aic_scores[i+1] <- AIC(seg)
    }
  }
  
  # Find the smallest AIC score (the best-fit model)
  min_score <- which.min(aic_scores)
  
  # If the min_score is index 1, then the best-fit model is linear
  if(min_score == 1){
    plot(dat,
         xlim = c(x_min, (x_max + 0.5)),
         ylim = c(y_min, y_buff),
         ylab = "Log Speciation Rate",
         xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
         main = "Speciation-Area Relationship",
         pch = 16)
    abline(linear)
    
    summary_line <- summary(linear)
    
    result <- list("summary" = summary_line, "linObj" = linear, "aggDF" = dat)
    
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
         ylim = c(y_min, y_buff),
         ylab = "Log Speciation Rate",
         xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
         main = "Speciation-Area Relationship")
    # Add the points
    points(dat$x, dat$y, pch = 19)
    
    # Save the summary as an object to add to the result list
    summary_seg <- summary(seg)
    
    result <- list("summary" = summary_seg, "segObj" = seg, "aggDF" = dat)
    
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
    
    result <- list("summary" = summary_seg, "segObj" = seg, "aggDF" = dat)
    
    class(result) <- "SAR"
    
    return(result)
  }
}
