# Read tree via extdata file
# tree <- ape::read.tree(system.file("extdata", 
#                               "Patton_Anolis_trimmed.tree", 
#                               package = "SSARP"))
# tree <- 
#   ape::read.tree(system.file("testthat/testdata/Patton_Anolis_trimmed.tree", 
#                              package = "SSARP"))
tree <- ape::read.tree("https://raw.githubusercontent.com/kmartinet/SSARP/refs/heads/main/vignettes/Patton_Anolis_Trimmed.tree")
# Tree from: Patton, A.H., Harmon, L.J., del Rosario Castañeda, M., 
#   Frank, H.K., Donihue, C.M., Herrel, A., & Losos, J.B. (2021). When adaptive 
#   radiations collide: Different evolutionary trajectories between and within 
#   island and mainland lizard clades. PNAS, 118(42): e2024451118.

# Create simple dataframe for testing with fictional area assignments
Species <- tree$tip.label
Genus <- rep("Anolis", length(Species))
areas <- c(rep(12000, 8), 
           rep(14000, 9),
           rep(15000, 10),
           rep(16000, 13),
           rep(13000, 9),
           rep(17000, 14),
           rep(19000, 34))
occs <- as.data.frame(cbind(Genus, Species, areas))

# Create a tree with binomial labels
tree_b <- tree
binom_names <- paste(Genus, Species, sep = " ")
tree_b$tip.label <- binom_names

# Matrix version of dataframe
occ_mat <- as.matrix(occs)

########

test_that("Inputting a matrix instead of a dataframe for occurrence records 
          will cause an error", {
  expect_error(estimate_MS(tree = tree, label_type = "epithet", 
                            occurrences = occ_mat))
})

test_that("Inputting a non-string for the label type will cause an error", {
  expect_error(estimate_MS(tree = tree, label_type = 1, occurrences = occs))
})

test_that("The estimate_MS function returns a dataframe (epithet labels)", {
  expect_s3_class(estimate_MS(tree = tree, label_type = "epithet", 
                               occurrences = occs), "data.frame")
})

test_that("The estimate_MS function returns a dataframe (binomial labels)", {
  expect_s3_class(estimate_MS(tree = tree_b, label_type = "binomial", 
                               occurrences = occs), "data.frame")
})

test_that("When a tree with binomial tip labels is used but the label type is 
          specified as 'epithet', an error will occur", {
  expect_error(estimate_MS(tree = tree_b, label_type = "epithet", 
                            occurrences = occs))
})
