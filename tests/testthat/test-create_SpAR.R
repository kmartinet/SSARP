# Zero breakpoints
areas_0 <- c(3125000,2.2e+07,28125000,33624999.9999999,44312499.9999999,
             44875000,121812500,1.26e+08,1.42e+08,180500000,212375000,228562500,
             263875000,268312500,309375000,321875000,329749999.999999,
             433687499.999999,528124999.999999,812937500.000001,1196500000,
             6117437500,9710687500.00002,12225750000,83104562500,1.22e+11)

rate_0 <- c(0.0643602672833435,0.0791427296997017,0.0665846190939991,
            0.0791427296997017,0.0791427296997017,0.0495741940555045,
            0.0347953424506271,0.0791427296997017,0.0565471464482032,
            0.0666943402964527,0.0702732522498868,0.0791427296997017,
            0.0720273676258257,0.0770764463984917,0.0466462555454143,
            0.0708478125440814,0.0791427296997017,0.0664720476285376,
            0.0758166756560211,0.0478709425239934,0.0684993567599238,
            0.0717514984915227,0.0492645998325333,0.0535781436258825,
            0.0520539540562288,0.0708707313219265)

occ_zero_bp <- as.data.frame(cbind(areas_0, rate_0))
colnames(occ_zero_bp) <- c("areas", "rate")

# One breakpoint
areas <- c(9710687500,301187500,12225750000,83104562500,1.19e+09,643812500,
           228562500,187500,1.22e+11,263875000,329750000,1196500000,14250000,
           1.26e+08,321875000,268312500,812937500,528125000,16937500,33625000,
           3125000,103625000,180500000,22812500,121812500,212375000,6117437500,
           28125000,2.2e+07,309375000,433687500,240937500,44875000,1765125000,
           1.42e+08,112375000,44312500,56062500)
rate <- c(1.0585377376259,1,1.04591607995137,1.03738198711277,1,1,1,1,1,1,1,1,
          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
occ_one_bp <- as.data.frame(cbind(areas, rate))

# Two breakpoints
rate_2 <- c(0, 0, 0, 0,
             0, 1,
             0, 1, 2,
             0, 1, 2, 3,
             0, 1, 2, 3, 4, 
             0, 1, 2, 3, 4,
             0, 1, 2, 3, 4,
             0, 1, 2, 3, 4)
rate_2 <- rate_2 + 1
areas_2 <- c(100, 200, 300, 400,
           500, 500,
           600, 600, 600,
           700, 700, 700, 700,
           800, 800, 800, 800, 800,
           900, 900, 900, 900, 900,
           1000, 1000, 1000, 1000, 1000,
           1100, 1100, 1100, 1100, 1100)
occ_two_bp <- as.data.frame(cbind(rate_2, areas_2))
colnames(occ_two_bp) <- c("rate", "areas")

# Matrix occ
occ_mat <- as.matrix(occ_one_bp)

# Occurrence record df with incorrect column names
occ_name <- occ_one_bp
colnames(occ_name) <- c("test1", "test2")

# Occurrence record df with correct column names, but incorrect types
occ_types <- occ_one_bp
occ_types$areas <- as.character(occ_types$areas)
occ_types$rate <- as.character(occ_types$rate)

########

test_that("Inputting a matrix instead of a dataframe causes an error", {
  expect_error(create_SpAR(occ_mat, npsi = 0))
})

test_that("Inputting a non-numeric into npsi throws an error", {
  expect_error(create_SpAR(occ_one_bp, npsi = "test"))
})

test_that("The create_SpAR function returns a list (zero breakpoints)", {
  expect_type(create_SpAR(occ_zero_bp, npsi = 0, visualize = TRUE), "list")
})

test_that("The create_SpAR function returns a list (one breakpoint)", {
  expect_type(create_SpAR(occ_one_bp, npsi = 1, visualize = TRUE), "list")
})

test_that("The create_SpAR function returns a list (two breakpoints)", {
  expect_type(create_SpAR(occ_two_bp, npsi = 2, visualize = TRUE), "list")
})

test_that("Inputting a dataframe without required column names causes an 
          error", {
            expect_error(create_SpAR(occ_name, npsi = 0))
})

test_that("Inputting a dataframe with required column names, but incorrect
          types causes an error", {
            expect_error(create_SpAR(occ_types, npsi = 0))
})

##### Ensuring calculations are correct #####
test_that("The slope is calculated correctly (zero breakpoints)", {
  model <- create_SpAR(occ_zero_bp, npsi = 0)
  slope <- round(model$summary$coefficients[2], digits = 4)
  expect_setequal(slope, -0.0171)
})

test_that("The slopes and breakpoint are calculated correctly 
          (one breakpoint)", {
            model <- create_SpAR(occ_one_bp, npsi = 1)
            slope1 <- round(model$summary$coefficients[2], digits = 4)
            slope2 <- round(model$summary$coefficients[3], digits = 4)
            bp <- round(model$summary$psi[2], digits = 4)
            expect_setequal(slope1, 0)
            expect_setequal(slope2, 0.0057)
            expect_setequal(bp, 19.9237)
})

test_that("The slopes and breakpoints are calculated correctly 
          (two breakpoints)", {
            model <- create_SpAR(occ_two_bp, npsi = 2)
            slope1 <- round(model$summary$coefficients[2], digits = 4)
            slope2 <- round(model$summary$coefficients[3], digits = 4)
            slope3 <- round(model$summary$coefficients[4], digits = 4)
            bp1 <- round(model$summary$psi[3], digits = 4)
            bp2 <- round(model$summary$psi[4], digits = 4)
            expect_setequal(slope1, 0)
            expect_setequal(slope2, 1.6385)
            expect_setequal(slope3, -1.6385)
            expect_setequal(bp1, 5.9811)
            expect_setequal(bp2, 6.6516)
})
