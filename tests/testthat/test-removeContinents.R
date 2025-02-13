# Test dataframe
occs <- as.data.frame(matrix(nrow = 4, ncol = 10))
names <- c("one", "two", "three", "four")
areas <- c(5.50e13, 3.04e13, 10, 15)
occs[,8] <- names
occs[,10] <- areas

# Test matrix
occs_mat <- matrix(nrow = 1, ncol = 1)
occs_mat[1,1] <- 5.50e13
colnames(occs_mat) <- "areas"

########

test_that("Inputting a matrix instead of a dataframe will cause an error", {
  expect_error(removeContinents(occs_mat))
})

test_that("removeContinents returns a dataframe", {
  expect_s3_class(removeContinents(occs), "data.frame")
})

