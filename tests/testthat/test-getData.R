# String inputs (error)
bad_key <- "testing"
bad_limit <- "one hundred"

# Regular inputs
key <- 8782549 # Anolis

########

test_that("Inputting a string as a key will cause an error", {
  expect_error(getData(bad_key))
})

test_that("Inputting a string as a limit will cause an error", {
  expect_error(getData(key = 1234, limit = bad_limit))
})

test_that("The getData function returns a dataframe when geometry = NULL", {
  expect_s3_class(getData(key = key, limit = 100, geometry = NULL), "data.frame")
})

test_that("The getData function returns a dataframe with valid geometry statement", {
  expect_s3_class(getData(key = key, limit = 100, geometry = 'POLYGON((-84.8 23.9, -84.7 16.4, -65.2 13.9, -63.1 11.0, -56.9 15.5, -60.5 21.9, -79.3 27.8, -79.8 24.8, -84.8 23.9))'), "data.frame")
})

test_that("When given an improper geometry statement, it will throw an error", {
  expect_error(getData(key = key, limit = 100, geometry = '1,2'))
})
