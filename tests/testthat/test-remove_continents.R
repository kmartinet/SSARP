# Test dataframe
occs <- as.data.frame(matrix(nrow = 4, ncol = 10))
names <- c("one", "two", "three", "four")
areas <- c(5.50e13, 3.04e13, 10, 15)
occs[,8] <- names
occs[,10] <- areas
colnames(occs) <- c("acceptedScientificName", "genericName", "specificEpithet", 
                    "decimalLongitude", "decimalLatitude", "First", "Second", 
                    "Third", "datasetKey", "areas")

# Test matrix
occs_mat <- matrix(nrow = 1, ncol = 1)
occs_mat[1,1] <- 5.50e13
colnames(occs_mat) <- "areas"

# Occurrence record df with incorrect column names
occ_name <- occs
colnames(occ_name) <- c(1:10)

# Occurrence record df with correct column names, but incorrect types
occ_types <- occs
occ_types$areas <- as.character(occ_types$areas)

########

test_that("Inputting a matrix instead of a dataframe will cause an error", {
  expect_error(remove_continents(occs_mat))
})

test_that("remove_continentsnts returns a dataframe", {
  expect_s3_class(remove_continents(occs), "data.frame")
})

test_that("Inputting a dataframe without the correct column names will cause
          an error", {
            expect_error(remove_continents(occ_name))
})

test_that("Inputting a dataframe without the correct types will cause an error",
          {
            expect_error(remove_continents(occ_types))
})
