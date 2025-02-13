# Test dataframe for findLand occs input without values
occs <- as.data.frame(matrix(ncol = 6, nrow = 2))
colnames(occs) <- c("decimalLongitude", "decimalLatitude", "acceptedScientificName", "genericName", "specificEpithet", "datasetKey")

# Test dataframe for findLand occs input with values
occs_vals <- occs
occs_vals[1,] <- c(-81.948509, 28.028463, "Anolis first", "Anolis", "first", 1)
occs_vals[2,] <- c(-81.949353, 28.028047, "Anolis second", "Anolis", "second", 1)

# Test matrix for findLand occs input
occ_mat <- matrix(ncol = 6, nrow = 2)
colnames(occ_mat) <- c("decimalLongitude", "decimalLatitude", "acceptedScientificName", "genericName", "specificEpithet", "datasetKey")
occ_mat[1,] <- c(-81.948509, 28.028463, "Anolis first", "Anolis", "first", 1)
occ_mat[2,] <- c(-81.949353, 28.028047, "Anolis second", "Anolis", "second", 1)

# Smaller dataframe for findLand occs input
occs_small <- occs_vals[,1:3]

# Dataframe with no rows for findLand occs input
occs <- as.data.frame(matrix(ncol = 6, nrow = 0))
colnames(occs) <- c("decimalLongitude", "decimalLatitude", "acceptedScientificName", "genericName", "specificEpithet", "datasetKey")

# Dataframe with an occurrence record point that map.where() doesn't have info for
# Test dataframe for findLand occs input with values
occs_mystery <- occs_vals
occs_mystery[1,] <- c(-90.66546, -0.611595, "Anolis first", "Anolis", "first", 1)

########

test_that("Inputting a matrix instead of a dataframe for occurrence records will cause an error", {
  expect_error(findLand(occ_mat))
})

test_that("Inputting a non-logical for the Fillgaps parameter will cause an error", {
  expect_error(findLand(occs_vals, fillgaps = 0))
})

test_that("The findLand function returns a dataframe", {
  expect_s3_class(findLand(occs_vals), "data.frame")
})

test_that("When occurrence record dataframe input has no rows, expect a warning message", {
  expect_message(findLand(occs), "Occurrence record dataframe has no entries")
})

test_that("Fillgaps attempts to get information for a datapoint unsupported by map.where", {
  expect_message(findLand(occs_mystery, fillgaps = TRUE), "Filling gaps using Photon...")
})

