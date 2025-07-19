# Test occurrence record dataframe
specificEpithet <- c("Tropius", "Taillow", "Chikorita", "Girafarig", 
                     "Octillery", "Amaura", "Pangoro", "Fearow", "Pikachu", 
                     "Weepinbell", "Graveler", "Voltorb")
areas <- c(650, 650, 800, 800, 800, 600, 600, 1000, 1000, 1000, 1000, 1000)
occ <- as.data.frame(cbind(specificEpithet, areas))
occ$areas <- as.numeric(occ$areas)

occ_mat <- as.matrix(occ)

# Occurrence record dataframe for single breakpoint
specificEpithet <- c("one", "one", "one", "one", "one", "one",
             "one", "two", "three",
             "one", "two", "three", "four", "five")
areas <- c(100, 100, 100, 200, 300, 400, 
           600, 600, 600,
           700, 700, 700, 700, 700)
occ_one_bp <- as.data.frame(cbind(specificEpithet, areas))
occ_one_bp$areas <- as.numeric(occ_one_bp$areas)

# Occurrence record dataframe for two breakpoints
specificEpithet <- c("one", "one", "one", "one",
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
occ_two_bp <- as.data.frame(cbind(specificEpithet, areas))
occ_two_bp$areas <- as.numeric(occ_two_bp$areas)

# Occurrence record df with incorrect column names
occ_name <- occ
colnames(occ_name) <- c("test1", "test2")

# Occurrence record df with correct column names, but incorrect types
occ_types <- occ
occ_types$specificEpithet <- as.factor(occ_types$specificEpithet)
occ_types$areas <- as.character(occ_types$areas)

########

test_that("The create_SAR function returns a list (zero breakpoints)", {
  expect_type(create_SAR(occ, npsi = 0), "list")
})

# Using suppressWarnings here because the test dataset is very simple, 
# and while the best-fit model is a regression with one breakpoint, 
# the "segemented" package gives warnings about the reliability of the results
test_that("The create_SAR function returns a list (one breakpoint)", {
  suppressWarnings(expect_type(create_SAR(occ_one_bp, npsi = 1), "list"))
})

test_that("The create_SAR function returns a list (two breakpoints)", {
  expect_type(create_SAR(occ_two_bp, npsi = 2), "list")
})

test_that("Inputting a matrix instead of a dataframe causes an error", {
  expect_error(create_SAR(occ_mat, npsi = 0))
})

test_that("Inputting a non-numeric into npsi throws an error", {
  expect_error(create_SAR(occ, npsi = "test"))
})

test_that("Inputting a dataframe without required column names causes an 
          error", {
            expect_error(create_SAR(occ_name, npsi = 0))
})

test_that("Inputting a dataframe with required column names, but incorrect
          types causes an error", {
            expect_error(create_SAR(occ_types, npsi = 0))
})

##### Ensuring calculations are correct #####
test_that("The slope is calculated correctly (zero breakpoints)", {
  model <- create_SAR(occ, npsi = 0)
  slope <- round(model$summary$coefficients[2], digits = 4)
  expect_setequal(slope, 1.8734)
})

# Using suppressWarnings here because the test dataset is very simple, 
# and while the best-fit model is a regression with one breakpoint, 
# the "segemented" package gives warnings about the reliability of the results
test_that("The slopes and breakpoint are calculated correctly 
          (one breakpoint)", {
          suppressWarnings(model <- create_SAR(occ_one_bp, npsi = 1))
          slope1 <- round(model$summary$coefficients[2], digits = 4)
          slope2 <- round(model$summary$coefficients[3], digits = 4)
          bp <- round(model$summary$psi[2], digits = 4)
          expect_setequal(slope1, 0)
          expect_setequal(slope2, 3.3138)
          expect_setequal(bp, 6.0654)
})

test_that("The slopes and breakpoints are calculated correctly 
          (two breakpoints)", {
          model <- create_SAR(occ_two_bp, npsi = 2)
          slope1 <- round(model$summary$coefficients[2], digits = 4)
          slope2 <- round(model$summary$coefficients[3], digits = 4)
          slope3 <- round(model$summary$coefficients[4], digits = 4)
          bp1 <- round(model$summary$psi[3], digits = 4)
          bp2 <- round(model$summary$psi[4], digits = 4)
          expect_setequal(slope1, 0)
          expect_setequal(slope2, 2.4763)
          expect_setequal(slope3, -2.4763)
          expect_setequal(bp1, 5.9677)
          expect_setequal(bp2, 6.6176)
 })
