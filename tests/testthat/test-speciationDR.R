# Read tree via raw file on SSARP's GitHub repo
tree <- ape::read.tree("https://raw.githubusercontent.com/kmartinet/SSARP/refs/heads/main/vignettes/Patton_Anolis_Trimmed.tree")

# The speciationDR function only really needs "Genus" and "Species" columns to work, so a simple dataframe can be created for testing (without real occurrence data)
Species <- tree$tip.label
Genus <- rep("Anolis", length(Species))
occ_simple <- as.data.frame(cbind(Genus, Species))

# Create a tree with binomial labels
tree_b <- tree
binom_names <- paste(Genus, Species, sep = " ")
tree_b$tip.label <- binom_names

# Matrix version of dataframe
occ_mat <- as.matrix(occ_simple)

########

test_that("Inputting a matrix instead of a dataframe for occurrence records will cause an error", {
  expect_error(speciationDR(tree = tree, label_type = "epithet", occurrences = occ_mat))
})

test_that("Inputting a non-string for the label type will cause an error", {
  expect_error(speciationDR(tree = tree, label_type = 1, occurrences = occ_simple))
})

test_that("The speciationDR function returns a dataframe (epithet labels)", {
  expect_s3_class(speciationDR(tree = tree, label_type = "epithet", occurrences = occ_simple), "data.frame")
})

test_that("The speciationDR function returns a dataframe (binomial labels)", {
  expect_s3_class(speciationDR(tree = tree_b, label_type = "binomial", occurrences = occ_simple), "data.frame")
})

test_that("When a tree with binomial tip labels is used but the label type is specified as 'epithet', the returned dataframe will be empty", {
  speciation_rates <- speciationDR(tree = tree_b, label_type = "epithet", occurrences = occ_simple)
  expect_length(speciation_rates[,1], 0)
})
