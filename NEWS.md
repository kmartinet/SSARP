SSARP 0.3.0 (2025-07-04)
=========================

### NEW FEATURES

  * Changed the names of all functions to "verb_object" structure
  * Two new example files were added to the package: `Patton_Anolis_Trimmed.tree` and `SSARP_Example_Dat.csv` to allow users to run examples involving a phylogenetic tree of *Anolis* and GBIF data for *Anolis*, respectively
  * Added "get_presence_absence" function, which creates a presence-absence matrix when given a dataframe output by `SSARP::find_areas()`

### DOCUMENTATION FIXES
  * Function names are now in `pkg::function()` notation throughout the documentation
  * Vignettes have been updated to reflect the new function names and example files
  * The majority of examples will now run, instead of remaining in a `\dontrun` block as in 0.2.0

### OTHER FIXES
  * `@import` and `@importFrom` statements were removed in favor of pkg::function() statements across the package

SSARP 0.2.0 (2025-04-29)
=========================

### NEW FEATURES

  * Added NEWS file
  
### DOCUMENTATION FIXES
  * Added badge for status at rOpenSci software peer review to README
  
