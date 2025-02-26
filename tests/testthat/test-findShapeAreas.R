# Create test SpatVector with WKT polygon
# This polygon is around the University of Arizona
test_SpatVector <- terra::vect("POLYGON ((-110.96 32.23, -110.96 32.22, -110.94 32.227, -110.94 32.234, -110.96 32.23))")
test_SpatVector$name <- "UA"
test_SpatVector$areas <- 791255.92 # m^2
test_SpatVector$featurecla <- "campus"

# Create test dataframe for findShapeAreas occs input
occs <- as.data.frame(matrix(ncol = 5, nrow = 2))
colnames(occs) <- c("decimalLatitude", "decimalLongitude", "genericName", "specificEpithet", "datasetKey")

# Test dataframe for findAreas occs input with values
occs_vals <- occs
occs_vals[1,] <- c(32.2318081, -110.9469145, "Aspidoscelis", "sonorae", 1)
occs_vals[2,] <- c(32.2292764, -110.9536270, "Aspidoscelis", "sonorae", 1)
occs_vals[,1] <- as.numeric(occs_vals[,1])
occs_vals[,2] <- as.numeric(occs_vals[,2])

########

test_that("The findShapeAreas function returns a list", {
  expect_type(findShapeAreas(occs_vals, test_SpatVector), "list")
})

