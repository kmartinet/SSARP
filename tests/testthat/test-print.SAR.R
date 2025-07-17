# Dataframe for linear object plotting
species_L <- c("Tropius", "Taillow", "Chikorita", "Girafarig", "Octillery", 
               "Amaura", "Pangoro", "Fearow", "Pikachu", "Weepinbell", 
               "Graveler", "Voltorb")
areas_L <- c(650, 650, 800, 800, 800, 600, 600, 1000, 1000, 1000, 1000, 1000)
occ_L <- as.data.frame(cbind(species_L, areas_L))
occ_L$areas_L <- as.numeric(occ_L$areas_L)
colnames(occ_L) <- c("specificEpithet", "areas")

# Occurrence record dataframe for single breakpoint
specificEpithet <- c("one", "one", "one", "one", "one", "one",
             "one", "two", "three",
             "one", "two", "three", "four", "five")
areas <- c(100, 100, 100, 200, 300, 400, 
           600, 600, 600,
           700, 700, 700, 700, 700)
occ_one_bp <- as.data.frame(cbind(specificEpithet, areas))
occ_one_bp$areas <- as.numeric(occ_one_bp$areas)

########

test_that("Using print.SAR on a linear SAR will print the linear model 
          summary, which is a list", {
  p <- create_SAR(occ_L, npsi = 0)
  expect_type(print(p), "list")
})

# Using suppressWarnings here because the test dataset is very simple, 
# and while the best-fit model is a regression with one breakpoint, 
# the "segemented" package gives warnings about the reliability of the results
test_that("Using print.SAR on a segmented SAR with one breakpoint will print 
          the segmented model summary, which is NULL", {
  suppressWarnings(p <- create_SAR(occ_one_bp, npsi = 1))
  suppressWarnings(expect_type(print(p), "NULL"))
})
