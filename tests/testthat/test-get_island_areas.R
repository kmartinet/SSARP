test_that("The get_island_areas function returns a dataframe", {
  expect_s3_class(get_island_areas(), "data.frame")
})

test_that("The get_island_areas function returns a dataframe with 5 columns", {
  expect_equal(ncol(get_island_areas()), 5)
})
