# Create test dataframe for occs input
occs <- as.data.frame(matrix(ncol = 10, nrow = 2))
colnames(occs) <- c("acceptedScientificName", "genericName", "specificEpithet", 
                    "decimalLongitude", "decimalLatitude", "First", 
                    "Second", "Third", "datasetKey", "areas")

# Test dataframe for find_areas occs input with values
occs_vals <- occs
occs_vals[1,] <- c("Anolis first_sp", "Anolis", "first_sp", -81.948509, 
                   28.028463, "USA", "Florida", "Lakeland", 1, 100)
occs_vals[2,] <- c("Anolis second_sp", "Anolis", "second_sp", -81.949353, 
                   28.028047, "USA", "Florida", "Lakeland", 1, 200)
occs_vals[,4] <- as.numeric(occs_vals[,4])
occs_vals[,5] <- as.numeric(occs_vals[,5])
occs_vals[,10] <- as.numeric(occs_vals[,10])

# Test matrix for occs input
occ_mat <- matrix(ncol = 10, nrow = 2)
colnames(occs) <- c("acceptedScientificName", "genericName", "specificEpithet", 
                    "decimalLongitude", "decimalLatitude", "First", 
                    "Second", "Third", "datasetKey", "areas")
occ_mat[1,] <- c("Anolis first_sp", "Anolis", "first_sp", -81.948509, 
                 28.028463, "USA", "Florida", "Lakeland", 1, 100)
occ_mat[2,] <- c("Anolis second_sp", "Anolis", "second_sp", -81.949353, 
                 28.028047, "USA", "Florida", "Lakeland", 1, 200)

########
test_that("Inputting a matrix instead of a dataframe for occurrence records 
          will cause an error", {
            expect_error(get_presence_absence(occ_mat))
          })

test_that("get_presence_absence returns a dataframe", {
  expect_s3_class(get_presence_absence(occs_vals), "data.frame")
})
