# Read tree via extdata file
# tree <- ape::read.tree(system.file("extdata", 
#                               "Patton_Anolis_trimmed.tree", 
#                               package = "ssarp"))
# tree <- 
#   ape::read.tree(system.file("testthat/testdata/Patton_Anolis_trimmed.tree", 
#                              package = "ssarp"))

if (!curl::has_internet()) {
  message("No internet connection. Cannot run estimate_MS tests without 
          downloading phylogenetic tree from GitHub.")
} else {
  tree <- ape::read.tree("https://raw.githubusercontent.com/kmartinet/ssarp/refs/heads/main/vignettes/Patton_Anolis_Trimmed.tree")
  # Tree from: Patton, A.H., Harmon, L.J., del Rosario Castañeda, M., 
  #   Frank, H.K., Donihue, C.M., Herrel, A., & Losos, J.B. (2021). When adaptive 
  #   radiations collide: Different evolutionary trajectories between and within 
  #   island and mainland lizard clades. PNAS, 118(42): e2024451118.
  
  # Create simple dataframe for testing with fictional area assignments
  specificEpithet <- tree$tip.label
  genericName <- rep("Anolis", length(specificEpithet))
  areas <- c(rep(12000, 8), 
             rep(14000, 9),
             rep(15000, 10),
             rep(16000, 13),
             rep(13000, 9),
             rep(17000, 14),
             rep(19000, 34))
  occs <- as.data.frame(cbind(genericName, specificEpithet, areas))
  occs$areas <- as.numeric(occs$areas)
  
  # Create a tree with binomial labels
  tree_b <- tree
  binom_names <- paste(genericName, specificEpithet, sep = " ")
  tree_b$tip.label <- binom_names
  
  # Matrix version of dataframe
  occ_mat <- as.matrix(occs)
  
  # Occurrence record df with incorrect column names
  occ_name <- occs
  colnames(occ_name) <- c("test1", "test2", "test3")
  
  # Occurrence record df with correct column names, but incorrect types
  occ_types <- occs
  occ_types$genericName <- as.factor(occ_types$genericName)
  occ_types$specificEpithet <- as.factor(occ_types$specificEpithet)
  occ_types$areas <- as.character(occ_types$areas)
  
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
  
  test_that("Inputting a dataframe without required column names causes an 
            error", {
              expect_error(estimate_MS(tree = tree,
                                       label_type = "epithet",
                                       occurrences = occ_name))
  })
  
  test_that("Inputting a dataframe with required column names, but incorrect
            types causes an error", {
              expect_error(estimate_MS(tree = tree,
                                       label_type = "epithet",
                                       occurrences = occ_types))
  })
  
  ##### Ensuring calculations are correct #####
  test_that("The minimum and maximum speciation rates are correct
                (The rest should also be correct if this is the case)", {
                  sp_ms <- estimate_MS(tree = tree, 
                                       label_type = "epithet", 
                                       occurrences = occs)
                  max <- round(max(sp_ms$rate), digits = 4)
                  min <- round(min(sp_ms$rate), digits = 4)
                  expect_setequal(max, 1.0696)
                  expect_setequal(min, 1)
  })
}