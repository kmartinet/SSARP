# Normal inputs
query <- "Anolis"
rank <- "genus"
key <- 8782549 # Anolis
limit <- 100
geometry <- 'POLYGON((-84.8 23.9, -84.7 16.4, -65.2 13.9, -63.1 11.0, 
-56.9 15.5, -60.5 21.9, -79.3 27.8, -79.8 24.8, -84.8 23.9))'
npsi <- 0

# Bad inputs
query_bad <- 1
rank_bad <- 1
key_bad <- "testing"
limit_bad <- "one hundred"

########
# Note: quick_create_SAR is a simple function that runs all of the other functions 
#  pertinent to making a species-area relationship. To this end, the test cases 
#  are borrowed from the test files for the other functions.

# get_key
test_that("Inputting a numeric query instead of a string causes an error", {
  expect_error(quick_create_SAR(query_bad, rank, limit = 100, geometry = geometry, 
                         npsi = npsi))
})

test_that("Inputting a numeric rank instead of a string causes an error", {
  expect_error(quick_create_SAR(query, rank_bad, limit = 100, geometry = geometry, 
                         npsi = npsi))
})

# get_data
test_that("Inputting a string as a limit will cause an error", {
  expect_error(quick_create_SAR(query, rank, limit = limit_bad, geometry = geometry, 
                         npsi = npsi))
})

test_that("When given an improper geometry statement, quick_create_SAR will 
          throw an error", {
  expect_error(quick_create_SAR(query, rank, limit = 100, geometry = '1,2', 
                         npsi = npsi))
})

# find_land
test_that("When quick_create_SAR gets to the find_land statement, expect a message", {
 expect_message(quick_create_SAR(query, rank, limit = 100, geometry = geometry, 
                          npsi = npsi), "Finding land")
})

# find_areas
test_that("When quick_create_SAR gets to the find_areas statement, expect a message", {
 expect_message(quick_create_SAR(query, rank, limit = 100, geometry = geometry, 
                          npsi = npsi), "Gathering areas")
})

# remove_continents
test_that("When continent = TRUE, expect a message about removing continents", {
 expect_message(quick_create_SAR(query, rank, limit = 100, geometry = geometry, 
                          continent = TRUE, npsi = npsi), 
                "Removing continental areas")
})

# quick_create_SAR
test_that("quick_create_SAR returns a list", {
 expect_type(quick_create_SAR(query, rank, limit = 100, geometry = geometry, 
                       continent = TRUE, npsi = npsi), "list")
})
