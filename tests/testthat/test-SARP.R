# Test occurrence record dataframe
Species <- c("Tropius", "Taillow", "Chikorita", "Girafarig", "Octillery", 
             "Amaura", "Pangoro", "Fearow", "Pikachu", "Weepinbell", 
             "Graveler", "Voltorb")
areas <- c(650, 650, 800, 800, 800, 600, 600, 1000, 1000, 1000, 1000, 1000)
occ <- as.data.frame(cbind(Species, areas))
occ$areas <- as.numeric(occ$areas)

occ_mat <- as.matrix(occ)

# Occurrence record dataframe for single breakpoint
Species <- c("one", "one", "one", "one", "one", "one",
             "one", "two", "three",
             "one", "two", "three", "four", "five")
areas <- c(100, 100, 100, 200, 300, 400, 
           600, 600, 600,
           700, 700, 700, 700, 700)
occ_one_bp <- as.data.frame(cbind(Species, areas))
occ_one_bp$areas <- as.numeric(occ_one_bp$areas)

# Occurrence record dataframe for two breakpoints
Species <- c("one", "one", "one", "one",
             "one", "two",
             "one", "two", "three",
             "one", "two", "three", "four",
             "one", "two", "three", "four", "five",
             "one", "two", "three", "four", "five",
             "one", "two", "three", "four", "five",
             "one", "two", "three", "four", "five")
areas <- c(100, 200, 300, 400,
           500, 500,
           600, 600, 600,
           700, 700, 700, 700,
           800, 800, 800, 800, 800,
           900, 900, 900, 900, 900,
           1000, 1000, 1000, 1000, 1000,
           1100, 1100, 1100, 1100, 1100)
occ_two_bp <- as.data.frame(cbind(Species, areas))
occ_two_bp$areas <- as.numeric(occ_two_bp$areas)

########

test_that("The SARP function returns a list (zero breakpoints)", {
  expect_type(SARP(occ, npsi = 0), "list")
})

# Using suppressWarnings here because the test dataset is very simple, 
# and while the best-fit model is a regression with one breakpoint, 
# the "segemented" package gives warnings about the reliability of the results
test_that("The SARP function returns a list (one breakpoint)", {
  suppressWarnings(expect_type(SARP(occ_one_bp, npsi = 1), "list"))
})

test_that("The SARP function returns a list (two breakpoints)", {
  expect_type(SARP(occ_two_bp, npsi = 2), "list")
})

test_that("Inputting a matrix instead of a dataframe causes an error", {
  expect_error(SARP(occ_mat, npsi = 0))
})

test_that("Inputting a non-numeric into npsi throws an error", {
  expect_error(SARP(occ, npsi = "test"))
})

