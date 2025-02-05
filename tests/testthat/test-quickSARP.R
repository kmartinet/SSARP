# Normal inputs
query <- "Anolis"
rank <- "genus"
key <- 8782549 # Anolis
limit <- 100
geometry <- 'POLYGON((-84.8 23.9, -84.7 16.4, -65.2 13.9, -63.1 11.0, -56.9 15.5, -60.5 21.9, -79.3 27.8, -79.8 24.8, -84.8 23.9))'

# Bad inputs
query_bad <- 1
rank_bad <- 1
key_bad <- "testing"
limit_bad <- "one hundred"

# Test dataframe for findLand occs input without values
occs <- as.data.frame(matrix(ncol = 5, nrow = 2))
colnames(occs) <- c("decimalLongitude", "decimalLatitude", "acceptedScientificName", "genericName", "specificEpithet")

# Test dataframe for findLand occs input with values
occs_vals <- occs
occs_vals[1,] <- c(-81.948509, 28.028463, "Anolis first", "Anolis", "first")
occs_vals[2,] <- c(-81.949353, 28.028047, "Anolis second", "Anolis", "second")

# Test matrix for findLand occs input
occ_mat <- matrix(ncol = 5, nrow = 2)
colnames(occ_mat) <- c("decimalLongitude", "decimalLatitude", "acceptedScientificName", "genericName", "specificEpithet")
occ_mat[1,] <- c(-81.948509, 28.028463, "Anolis first", "Anolis", "first")
occ_mat[2,] <- c(-81.949353, 28.028047, "Anolis second", "Anolis", "second")

# Smaller dataframe for findLand occs input
occs_small <- occs_vals[,1:3]

# Dataframe with no rows for findLand occs input
occs <- as.data.frame(matrix(ncol = 5, nrow = 0))
colnames(occs) <- c("decimalLongitude", "decimalLatitude", "acceptedScientificName", "genericName", "specificEpithet")

# Dataframe with an occurrence record point that map.where() doesn't have info for
# Test dataframe for findLand occs input with values
occs_mystery <- occs_vals
occs_mystery[1,] <- c(-90.66546, -0.611595, "Anolis first", "Anolis", "first")

########
# Note: quickSARP is a simple function that runs all of the other functions pertinent to making a species-area relationship. To this end, the test cases are borrowed from the test files for the other functions.

# getKey
test_that("Inputting a numeric query instead of a string causes an error", {
  expect_error(quickSARP(query_bad, rank, limit = 100, geometry = geometry))
})

test_that("Inputting a numeric rank instead of a string causes an error", {
  expect_error(quickSARP(query, rank_bad, limit = 100, geometry = geometry))
})

# getData
test_that("Inputting a string as a limit will cause an error", {
  expect_error(quickSARP(query, rank, limit = limit_bad, geometry = geometry))
})

test_that("When given an improper geometry statement, quickSARP will throw an error", {
  expect_error(quickSARP(query, rank, limit = 100, geometry = '1,2'))
})

# findLand
test_that("When quickSARP gets to the findLand statement, expect a message", {
 expect_message(quickSARP(query, rank, limit = 100, geometry = geometry), "Finding land")
})

# findAreas
test_that("When quickSARP gets to the findAreas statement, expect a message", {
 expect_message(quickSARP(query, rank, limit = 100, geometry = geometry), "Gathering areas")
})

# removeContinents
test_that("When continent = TRUE, expect a message about removing continents", {
 expect_message(quickSARP(query, rank, limit = 100, geometry = geometry, continent = TRUE), "Removing continental areas")
})

# quickSARP
test_that("quickSARP returns a list", {
 expect_type(quickSARP(query, rank, limit = 100, geometry = geometry, continent = TRUE), "list")
})
