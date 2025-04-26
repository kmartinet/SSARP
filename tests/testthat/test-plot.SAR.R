# Dataframe for linear object plotting
species_L <- c("Tropius", "Taillow", "Chikorita", "Girafarig", "Octillery", 
               "Amaura", "Pangoro", "Fearow", "Pikachu", "Weepinbell", 
               "Graveler", "Voltorb")
areas_L <- c(650, 650, 800, 800, 800, 600, 600, 1000, 1000, 1000, 1000, 1000)
occ_L <- as.data.frame(cbind(species_L, areas_L))
occ_L$areas_L <- as.numeric(occ_L$areas_L)
colnames(occ_L) <- c("Species", "areas")

# Occurrence record dataframe for single breakpoint
Species <- c("one", "one", "one", "one", "one", "one",
             "one", "two", "three",
             "one", "two", "three", "four", "five")
areas <- c(100, 100, 100, 200, 300, 400, 
           600, 600, 600,
           700, 700, 700, 700, 700)
occ_one_bp <- as.data.frame(cbind(Species, areas))
occ_one_bp$areas <- as.numeric(occ_one_bp$areas)


########

test_that("plot.SAR will plot a linear regression (type will be NULL)", {
  p <- SARP(occ_L, npsi = 0)
  expect_type(plot(p), "NULL")
})

# Using suppressWarnings here because the test dataset is very simple, 
# and while the best-fit model is a regression with one breakpoint, 
# the "segemented" package gives warnings about the reliability of the results
test_that("plot.SAR will plot a segmented regression (type will be NULL)", {
  suppressWarnings(p <- SARP(occ_one_bp, npsi = 1))
  suppressWarnings(expect_type(plot(p), "NULL"))
})
