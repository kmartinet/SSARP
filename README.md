# SSARP - IN BETA

SSARP (Species-/Speciation-Area Relationship Projector) is an R package that provides a suite of functions to help users **create speciation- (under development) and species-area relationships for island-dwelling taxa** using occurrence data from GBIF (Global Biodiversity Information Facility) accessed via GBIF's API or the user's own occurrence data.

### Installation

To install SSARP, use the "install_github" function from the *devtools* package:

library(devtools)

install_github("kmartinet/SSARP")

### Basic Workflow - Using data from GBIF to create a species-area relationship plot

1. Use **getKey(query, rank)** to find the unique identifying key associated with your taxon of interest.  
2. Use **getData(key, limit)** with the key found in Step 1 to access *limit* occurrence points from GBIF. There is a hard limit of 100,000 records for each call to **getData()**, as set by [*rgbif*](https://www.gbif.org/tool/81747/rgbif).  
3. Use **findLand(occ)** with the dataframe obtained in Step 2 to figure out the names of landmasses using the occurrence record GPS points and the [*maps* R package](https://cran.r-project.org/web/packages/maps/index.html).  
4. Use **findAreas(occ)** with the dataframe obtained in Step 3 to match the landmass names to a dataset that includes names of most islands on the planet and their areas.
4a. If you'd like to only include occurrence records from islands, you can remove continental records by using **removeContinents(occ)** with the dataframe returned by **findAreas()**
5. Use **SARP(occ, npsi)** with the dataframe obtained in Step 4 to create a species-area relationship plot that reports information important to the associated regression. The *npsi* parameter indicates how many breakpoints the user would like the segmented regression to include. A near-future update of this package will include automatic model selection for the optimal npsi when the user is unsure of how many breakpoints are appropriate for their occurrence dataset.

### (Currently only on the dev branch) Use data from GBIF and a user-provided phylogenetic tree to create a speciation-area relationship plot

1. Use **getKey(query, rank)** to find the unique identifying key associated with your taxon of interest.
2. Use **getData(key, limit)** with the key found in Step 1 to access *limit* occurrence points from GBIF. There is a hard limit of 100,000 records for each call to **getData()**, as set by [*rgbif*](https://www.gbif.org/tool/81747/rgbif).
3. Use **findLand(occ)** with the dataframe obtained in Step 2 to figure out the names of landmasses using the occurrence record GPS points and the [*maps* R package](https://cran.r-project.org/web/packages/maps/index.html).
4. Use **findAreas(occ)** with the dataframe obtained in Step 3 to match the landmass names to a dataset that includes names of most islands on the planet and their areas.
5. Use **speciationDR(tree, occ)** with your own phylogenetic tree that corresponds with the taxa signified in previous steps, along with the dataframe obtained in Step 4 to add tip speciation rates using the DR statistic (Jetz et al. 2012) to the occurrence dataframe.
6. Use **SpeARP(occ, npsi)** with the dataframe obtained in Step 5 to create a speciation-area relationship plot that reports information important to the associated regression. The *npsi* parameter indicates the maximum number of breakpoints the user would like to compare for model selection. The returned model and plot correspond with the best-fit model.

### Some helpful notes about well-known text (WKT) representation of geometry
When running getData(), the user can specify a well-known text (WKT) representation of geometry to restrict the geographic location of the returned occurrence records (currently only on the dev branch). The rgbif::occ_search function that getData() calls requires a counter-clockwise winding order for WKT. I find it helpful to think about WKT polygons in this way: imagine a square around your geographic area of interest and pick one of the corners as a starting point. The order of points in WKT format should follow counter-clockwise from the corner you picked first, and the final entry in the WKT string needs to be the same as the first entry. Additionally, while GPS points are typcially represented in "latitude, longitude" format, WKT expects them in "longitude latitude" format with commas separating the points rather than individual longitude and latitude values. WKT polygons can have more specified points than included in this simple square example, and even include polygons nested within others or polygons with holes in the middle. 

#### Literature Cited
Jetz, W., Thomas, G.H, Joy, J.B., Harmann, K., & Mooers, A.O. 2012. The global diversity of birds in space and time. *Nature* 491: 444-448.
