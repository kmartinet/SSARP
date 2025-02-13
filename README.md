# SSARP
  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/kmartinet/SSARP/branch/main/graph/badge.svg)](https://codecov.io/gh/kmartinet/SSARP?branch=main)
  [![R-CMD-check](https://github.com/kmartinet/SSARP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kmartinet/SSARP/actions/workflows/R-CMD-check.yaml)
  [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  <!-- badges: end -->

SSARP (Species-/Speciation-Area Relationship Projector) is an R package that provides a suite of functions to help users **create speciation- and species-area relationships for island-dwelling taxa** using occurrence data from GBIF (Global Biodiversity Information Facility) accessed via GBIF's API or the user's own occurrence data.

Please find the [bioRxiv preprint of the manuscript associated with *SSARP* here!](https://www.biorxiv.org/content/10.1101/2024.12.31.630948v1)

### Installation

To install *SSARP*, use the "install_github" function from the *devtools* package:
```
library(devtools)

install_github("kmartinet/SSARP")
```

### Basic Workflow - Using data from GBIF to create a species-area relationship plot

1. Use `getKey(query, rank)` to find the unique identifying key associated with your taxon of interest.  
2. Use `getData(key, limit, geometry)` with the key found in Step 1 to access *limit* occurrence points from GBIF. There is a hard limit of 100,000 records for each call to `getData()`, as set by [*rgbif*](https://www.gbif.org/tool/81747/rgbif). The "geometry" parameter can be specified with a well-known text (WKT) representation of of geometry to restrict the geographic location of the returned occurrence records (see more about WKT below).
3. Use `findLand(occ, fillgaps)` with the dataframe obtained in Step 2 to figure out the names of landmasses using the occurrence record GPS points and the [*maps* R package](https://cran.r-project.org/web/packages/maps/index.html).  Setting the "fillgaps" parameter to `TRUE` will enable the use of [Photon API](https://photon.komoot.io/) to fill in any missing landmass names left by the *maps* R package.
4. Use `findAreas(occ, area_custom)` with the dataframe obtained in Step 3 to match the landmass names to a dataset that includes names of most islands on the planet and their areas. If the user would like to use a custom island area dataset instead of the built-in one, the "area_custom" parameter can be set to the name of the custom island area dataframe.
4a. If you'd like to only include occurrence records from islands, you can remove continental records by using `removeContinents(occ)` with the dataframe returned by `findAreas()`
5. Use `SARP(occ, npsi)` with the dataframe obtained in Step 4 to create a species-area relationship plot that reports information important to the associated regression. The "npsi" parameter indicates the maximum number of breakpoints the user would like to compare for model selection. The returned model and plot correspond with the best-fit model.

### Use data from GBIF and a user-provided phylogenetic tree to create a speciation-area relationship plot

1. Use `getKey(query, rank)` to find the unique identifying key associated with your taxon of interest.  
2. Use `getData(key, limit, geometry)` with the key found in Step 1 to access *limit* occurrence points from GBIF. There is a hard limit of 100,000 records for each call to `getData()`, as set by [*rgbif*](https://www.gbif.org/tool/81747/rgbif). The "geometry" parameter can be specified with a well-known text (WKT) representation of of geometry to restrict the geographic location of the returned occurrence records (see more about WKT below).
3. Use `findLand(occ, fillgaps)` with the dataframe obtained in Step 2 to figure out the names of landmasses using the occurrence record GPS points and the [*maps* R package](https://cran.r-project.org/web/packages/maps/index.html).  Setting the "fillgaps" parameter to `TRUE` will enable the use of [Photon API](https://photon.komoot.io/) to fill in any missing landmass names left by the *maps* R package.
4. Use `findAreas(occ, area_custom)` with the dataframe obtained in Step 3 to match the landmass names to a dataset that includes names of most islands on the planet and their areas. If the user would like to use a custom island area dataset instead of the built-in one, the "area_custom" parameter can be set to the name of the custom island area dataframe.
4a. If you'd like to only include occurrence records from islands, you can remove continental records by using `removeContinents(occ)` with the dataframe returned by `findAreas()`
5. Use either `speciationDR(tree, label_type, occ)` or `speciationMS(tree, label_type, occ)` with your own phylogenetic tree that corresponds with the taxa signified in previous steps, a classifier that describes your tip labels (whether the tip labels are simply species epithets or full scientific names), and the dataframe obtained in Step 4 to add tip speciation rates using the DR statistic (Jetz et al. 2012) or the lambda calculation for crown groups from Magallόn and Sanderson (2001) respectively to the occurrence dataframe. The user may also choose to estimate tip speciation rates from a BAMM analysis (Rabosky 2014) by using `speciationBAMM(label_type, occ, edata)` with a classifier that describes your tip labels (whether the tip labels are simply species epithets or full scientific names), the occurrence record dataframe obtained in Step 4, and a bammdata object generated by reading the event data file from a BAMM analysis with the *BAMMtools* package (Rabosky et al. 2014).
6. Use `SpeARP(occ, npsi)` with the dataframe obtained in Step 5 to create a speciation-area relationship plot that reports information important to the associated regression. The "npsi" parameter indicates the maximum number of breakpoints the user would like to compare for model selection. The returned model and plot correspond with the best-fit model.

### Some helpful notes about well-known text (WKT) representation of geometry
When running `getData()`, the user can specify a well-known text (WKT) representation of geometry to restrict the geographic location of the returned occurrence records. The rgbif::occ_search function that `getData()` calls requires a counter-clockwise winding order for WKT. I find it helpful to think about WKT polygons in this way: imagine a square around your geographic area of interest and pick one of the corners as a starting point. The order of points in WKT format should follow counter-clockwise from the corner you picked first, and the final entry in the WKT string needs to be the same as the first entry. Additionally, while GPS points are typcially represented in "latitude, longitude" format, WKT expects them in "longitude latitude" format with commas separating the points rather than individual longitude and latitude values. WKT polygons can have more specified points than included in this simple square example, and even include polygons nested within others or polygons with holes in the middle. 

#### Literature Cited
- Jetz, W., Thomas, G.H, Joy, J.B., Harmann, K., & Mooers, A.O. (2012). The global diversity of birds in space and time. *Nature*, 491: 444-448.
- Magallόn, S. & Sanderson, M.J. (2001). Absolute Diversification Rates in Angiosperm Clades. *Evolution*, 55(9): 1762-1780.
- Rabosky, D.L. (2014). Automatic Detection of Key Innovations, Rate Shifts, and Diversity-Dependence on Phylogenetic Trees. PLOS ONE, 9(2): e89543.436
- Rabosky, D.L., Grundler, M., Anderson, C., Title, P., Shi, J.J., Brown, J.W., Huang, H., & Larson, J.G. (2014). BAMMtools: an R package for the analysis of evolutionary dynamics on phylogenetic trees. Methods in Ecology and Evolution, 5: 701-707.
