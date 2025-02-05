# Test matrix for getSources occs input
occ_mat <- matrix(ncol = 6, nrow = 2)
colnames(occ_mat) <- c("decimalLongitude", "decimalLatitude", "acceptedScientificName", "genericName", "specificEpithet", "datasetKey")
occ_mat[1,] <- c(-81.948509, 28.028463, "Anolis first", "Anolis", "first", 12345)
occ_mat[2,] <- c(-81.949353, 28.028047, "Anolis second", "Anolis", "second", 54321)

# Test dataframe
occ_df <- as.data.frame(occ_mat)

# Test dataframe missing datasetKey
occs_short <- occ_df[,-6]

########

test_that("Inputting a matrix instead of a dataframe for occurrence records will cause an error", {
  expect_error(getSources(occ_mat))
})

test_that("getSources returns a dataframe", {
  expect_s3_class(getSources(occ_df), "data.frame")
})

test_that("When occurrence record dataframe input does not have the datasetKey column, expect a warning message", {
  expect_message(getSources(occs_short), "datasetKey column not found.")
})

