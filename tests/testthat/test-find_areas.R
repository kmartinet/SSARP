# Create test dataframe for find_areas occs input
occs <- as.data.frame(matrix(ncol = 9, nrow = 2))
colnames(occs) <- c("SpeciesName", "Genus", "Species", "Longitude", 
                    "Latitude", "First", "Second", "Third", "datasetKey")

# Test dataframe for find_areas occs input with values
occs_vals <- occs
occs_vals[1,] <- c("Anolis first", "Anolis", "first", -81.948509, 28.028463, 
                   "USA", "Florida", "Lakeland", 1)
occs_vals[2,] <- c("Anolis second", "Anolis", "second", -81.949353, 28.028047, 
                   "USA", "Florida", "Lakeland", 1)
occs_vals[,4] <- as.numeric(occs_vals[,4])
occs_vals[,5] <- as.numeric(occs_vals[,5])

# Test dataframe for find_areas custom area input
custom_area <- as.data.frame(matrix(ncol = 2, nrow = 2))
colnames(custom_area) <- c("Name", "Area")

# Test matrix for find_areas occs input
occ_mat <- matrix(ncol = 9, nrow = 2)
colnames(occ_mat) <- c("SpeciesName", "Genus", "Species", "Longitude", 
                       "Latitude", "First", "Second", "Third", "datasetKey")
occ_mat[1,] <- c("Anolis first", "Anolis", "first", -81.948509, 28.028463, 
                 "USA", "Florida", "Lakeland", 1)
occ_mat[2,] <- c("Anolis second", "Anolis", "second", -81.949353, 28.028047, 
                 "USA", "Florida", "Lakeland", 1)

# Smaller dataframe for find_areas occs input
occs_small <- occs_vals[,1:5]

# Uniq_islands to test full find_areas grep sequence
uniq_islands <- c("Aappalaartoq", "Abbotts Harbour", "A Chau Island")

########

test_that("Inputting an empty custom area dataframe will result in an empty 
          dataframe", {
 test_areas <- find_areas(occs_vals, custom_area)
 expect_equal(length(test_areas[,1]), 0)
})

test_that("Inputting a matrix instead of a dataframe for occurrence records 
          will cause an error", {
  expect_error(find_areas(occ_mat))
})

test_that("Inputting a dataframe with insufficient columns will cause 
          an error", {
  expect_error(find_areas(occs_small))
})

test_that("The find_areas function returns a dataframe", {
  expect_s3_class(find_areas(occs_vals), "data.frame")
})

test_that("The find_areas function returns a dataframe with 10 columns", {
  expect_equal(ncol(find_areas(occs_vals)), 10)
})
