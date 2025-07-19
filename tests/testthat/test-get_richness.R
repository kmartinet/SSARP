# Occurrence record dataframe for single breakpoint (from test-create_SAR)
specificEpithet <- c("one", "one", "one", "one", "one", "one",
                     "one", "two", "three",
                     "one", "two", "three", "four", "five")
areas <- c(100, 100, 100, 200, 300, 400, 
           600, 600, 600,
           700, 700, 700, 700, 700)
occ <- as.data.frame(cbind(specificEpithet, areas))
occ$areas <- as.numeric(occ$areas)

# Create matrix
occ_mat <- as.matrix(occ)

# Occurrence record df with incorrect column names
occ_name <- occ
colnames(occ_name) <- c("test1", "test2")

# Occurrence record df with correct column names, but incorrect types
occ_types <- occ
occ_types$specificEpithet <- as.factor(occ_types$specificEpithet)
occ_types$areas <- as.character(occ_types$areas)

########
test_that("Inputting a matrix instead of a dataframe for occurrence records 
          will cause an error", {
            expect_error(get_richness(occ_mat))
})

test_that("get_richness returns a dataframe", {
  expect_s3_class(get_richness(occ), "data.frame")
})

test_that("Inputting a dataframe without the correct column names will cause
          an error", {
            expect_error(get_ricness(occ_name))
})

test_that("Inputting a dataframe without the correct types will cause an error",
          {
            expect_error(get_richness(occ_types))
})
