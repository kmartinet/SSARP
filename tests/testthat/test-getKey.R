# Normal inputs
query <- "Anolis"
rank <- "genus"

# Bad inputs
query_bad <- 1
rank_bad <- 1



test_that("Inputting a numeric query instead of a string causes an error", {
  expect_error(getKey(query_bad, rank))
})

test_that("Inputting a numeric rank instead of a string causes an error", {
  expect_error(getKey(query, rank_bad))
})

test_that("The getKey function returns a numeric with a valid query and rank", {
  expect_type(getKey(query, rank), "double")
})

test_that("The getKey function returns NA with a query and rank with no matches", {
  expect_equal(getKey(query = "Pikachu", rank = "family"), NA)
})

test_that("A message will be displayed when the name associated with the returned key does not match the provided query.", {
 expect_message(getKey("Anol", "genus"))
})
