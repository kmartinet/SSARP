# Read tree via extdata file
tree <- ape::read.tree(system.file("extdata", 
                              "Patton_Anolis_trimmed.tree", 
                              package = "SSARP"))
# Tree from: Patton, A.H., Harmon, L.J., del Rosario Castañeda, M., 
#   Frank, H.K., Donihue, C.M., Herrel, A., & Losos, J.B. (2021). When adaptive 
#   radiations collide: Different evolutionary trajectories between and within 
#   island and mainland lizard clades. PNAS, 118(42): e2024451118.

# Create an incomplete bammdata object for testing purposes
edata_test <- list()
meanTipLambda <- c(0.0314108259872537,0.0314108259872537,0.0318258913730437,
                   0.03144335221441,0.0314108259872537,0.0314108259872537,
                   0.0319306951007454,0.0314108259872537,0.0314108259872537,
                   0.0314108259872537,0.0314137179377825,0.0314108259872537,
                   0.0316396662025438,0.0314108259872537,0.0314108259872537,
                   0.0314108259872537,0.0314108259872537,0.0314108259872537,
                   0.0314108259872537,0.0314108259872537,0.0314108259872537,
                   0.0314108259872537,0.0314108259872537,0.032870201417437,
                   0.0330180119157737,0.0314108259872537,0.0318909050724412,
                   0.0314108259872537,0.0314108259872537,0.0314108259872537,
                   0.0314108259872537,0.0314108259872537,0.0315547519083308,
                   0.0314108259872537,0.0314839583518265,0.0314108259872537,
                   0.0314108259872537,0.0314108259872537,0.0323788824122355,
                   0.0314108259872537,0.0314108259872537,0.0316560442524905,
                   0.0324295398677694,0.0315016882879797,0.0315016882879797,
                   0.0314108259872537,0.0314108259872537,0.0314108259872537,
                   0.0314108259872537,0.0320609214388835,0.0314571702518079,
                   0.0314571702518079,0.0314108259872537,0.0316097744457865,
                   0.0314108259872537,0.0314108259872537,0.0315747939442214,
                   0.0314108259872537,0.0314142705326598,0.0314108259872537,
                   0.0314108259872537,0.0314108259872537,0.0314108259872537,
                   0.0314108259872537,0.0313877030905006,0.0314108259872537,
                   0.0314108259872537,0.0320286180397744,0.0314108259872537,
                   0.0314108259872537,0.0314108259872537,0.0314108259872537,
                   0.031389785464817,0.0314108259872537,0.0314108259872537,
                   0.0321195141671994,0.0315143469248042,0.0316239242155464,
                   0.0314108259872537,0.0314108259872537,0.0314108259872537,
                   0.031385471017099,0.0314108259872537,0.0314108259872537,
                   0.0386096160222734,0.0386096160222734,0.0386096160222734,
                   0.0386096160222734,0.0386096160222734,0.0313900718979742,
                   0.0314108259872537,0.032437479931859)
tip.label <- c('grahami','garmani','opalinus','reconditus','lineatopus',
               'valencienni','bremeri','quadriocellifer','sagrei','ophiolepis',
               'mestrei','confusus','homolechis','jubar','guafe','imias',
               'allogus','ahli','rubribarbus','cristatellus','cooki','krugi',
               'pulchellus','gundlachi','poncensis','evermanni','distichus',
               'caudalis','marron','brevirostris','websteri','armouri',
               'shrevei','breslini','strahmi','longitibialis','marcanoi',
               'allisoni','porcatus','oporinus','altitudinalis','isolepis',
               'guazuma','placidus','sheplani','centralis','argillaceus',
               'angusticeps','paternus','alayoni','cyanopleurus','cupeyalensis',
               'clivicola','vanidicus','alfaroi','macilentus','inexpectatus',
               'alutaceus','lucius','argenteolus','porcus','chamaeleonides',
               'barbatus','barahonae','baleatus','ricordii','eugenegrahami',
               'christophei','cuvieri','insolitus','fowleri','etheridgei',
               'semilineatus','alumina','olssoni','barbouri','hendersoni',
               'dolichocephalus','bahorucoensis','monticola','darlingtoni',
               'singularis','aliniger','chlorocyanus','smallwoodi','noblei',
               'baracoae','luteogularis','equestris','bartschi','vermiculatus',
               'occultus')
edata_test[['meanTipLambda']] <- meanTipLambda
edata_test[['tip.label']] <- tip.label
class(edata_test) <- 'bammdata'

# The occurrence record input only needs "Genus" and "Species" columns, 
#  so a simple dataframe can be created for testing 
#  (without real occurrence data)
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

test_that("Inputting a matrix instead of a dataframe for occurrence records 
          will cause an error", {
  expect_error(speciationBAMM(label_type = "epithet", 
                              occurrences = occ_mat, edata = edata_test))
})

test_that("Inputting a non-string for the label type will cause an error", {
  expect_error(speciationBAMM(label_type = 1, 
                              occurrences = occ_simple, edata = edata_test))
})

test_that("The speciationBAMM function returns a dataframe (epithet labels)", {
  expect_s3_class(speciationBAMM(label_type = "epithet", 
                                 occurrences = occ_simple, 
                                 edata = edata_test), "data.frame")
})

test_that("The speciationBAMM function returns a dataframe (binomial labels)", {
  expect_s3_class(speciationBAMM(label_type = "binomial", 
                                 occurrences = occ_simple, 
                                 edata = edata_test), "data.frame")
})
