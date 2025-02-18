# SSARP
  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/kmartinet/SSARP/branch/main/graph/badge.svg)](https://codecov.io/gh/kmartinet/SSARP?branch=main)
  [![R-CMD-check](https://github.com/kmartinet/SSARP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kmartinet/SSARP/actions/workflows/R-CMD-check.yaml)
  [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  <!-- badges: end -->

SSARP (Species-/Speciation-Area Relationship Projector) is an R package that provides a suite of functions to help users **create speciation- and species-area relationships for island-dwelling taxa** using occurrence data from GBIF (Global Biodiversity Information Facility) accessed via GBIF's API or the user's own occurrence data. 

Species- and speciation-area relationships are useful for determining whether biodiversity is distributed in an unusual way in a given island system (e.g., species richness decreasing as islands get larger instead of increasing). An unusual trend in these relationships may be indicative of habitat loss or a negative impact of non-native species.

Please find the [bioRxiv preprint of the manuscript associated with *SSARP* here!](https://www.biorxiv.org/content/10.1101/2024.12.31.630948v1)

### Installation

To install *SSARP*, use the "install_github" function from the *devtools* package:
```
library(devtools)

install_github("kmartinet/SSARP")
```

### Example: Creating a Species-Area Relationship
A species-area relationship (SAR) visualizes the relationship between species richness (the number of species) and the area of the land mass on which the species live. This brief example covers the *SSARP* workflow for creating a SAR, and more detailed explanations of the code and methods can be found [in the Articles on the SSARP pkgdown website](https://kmartinet.github.io/SSARP/index.html).

In order to construct a species-area relationship with *SSARP*, we will:

- Gather occurrence data from GBIF
- Filter out invalid occurrence records
- Find areas of pertinent land masses
- Create a species-area relationship

#### Step 1: Gather occurrence data from GBIF
In this step, we will find the unique identifying key associated with a taxon of interest (the lizard genus *Anolis* in this case) and use that key to access occurrence points for that taxon from GBIF. We will restrict the returned occurrence records to the Caribbean islands using the "geometry" parameter in the "getData" function.

```
library(SSARP)

# Get the GBIF key for the Anolis genus
key <- getKey(query = "Anolis", rank = "genus")

# Get data for Anolis from GBIF in a specified polygon around Caribbean islands
dat <- getData(key = key, limit = 10000,
  geometry = 'POLYGON((-84.8 23.9, -84.7 16.4, -65.2 13.9, -63.1 11.0, -56.9 15.5, -60.5 21.9, -79.3 27.8, -79.8 24.8, -84.8 23.9))')
```

#### Step 2: Filter out invalid occurrence records
Now that we have a dataframe that includes the first 10,000 records for *Anolis* within a specified polygon around the Caribbean islands, we will filter that data to include only occurrence records that are on land. Some occurrence records might have GPS points that are in the ocean instead of on an island, so it is important to exclude these invalid records.

```
# Find land mass names and exclude records not on land
land_dat <- findLand(occurrences = dat)
```

#### Step 3: Find areas of pertinent land masses
Next, we will use the dataframe of occurrence records with their associated land mass names (the "land_dat" object created above) with the "findAreas" function to find the area of each land mass.
```
# Use the land mass names to get their areas
area_dat <- findAreas(occs = land_dat)
```

To ensure that occurrence records included in our SAR are all on islands, we will use the "removeContinents" function to remove any continental occurrence records. While we specified a polygon around Caribbean islands in Step 1, some continental occurrence records may have been accidentally included due to user error when specifying the WKT polygon.

```
nocont_dat <- removeContinents(occs = area_dat)
```

#### Step 4: Create a species-area relationship
Finally, we will use the "nocont_dat" object created in Step 3 with the "SARP" function to create a SAR for *Anolis* lizards found on islands in the Caribbean. The "SARP" function creates multiple regression objects with breakpoints up to the user-specified "npsi" parameter. For example, if "npsi" is two, "SARP" will generate regression objects with zero (linear regression), one, and two breakpoints. The function will then return the regression object with the lowest AIC score. The "npsi" parameter will be set to one in this example.

```
SARP(occurrences = nocont_dat, npsi = 1)
```
![alt text](https://github.com/kmartinet/SSARP/blob/main/vignettes/anolis_SAR-1.png?raw=true)

This is the species-area relationship (SAR) for Anolis including island-based occurrences within a polygon around Caribbean islands from the first 10000 records for the genus in GBIF! The best-fit model was a segmented regression with one breakpoint. The R console will also output statistical information about the model:

```
## 
##  ***Regression Model with Segmented Relationship(s)***
## 
## Call: 
## segmented.lm(obj = linear, seg.Z = ~x, npsi = 1, control = seg.control(display = FALSE))
## 
## Estimated Break-Point(s):
##           Est. St.Err
## psi1.x 21.982  0.723
## 
## Coefficients of the linear terms:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.84251    1.00776  -0.836    0.409
## x            0.08500    0.05405   1.573    0.125
## U1.x         0.71424    0.21612   3.305       NA
## 
## Residual standard error: 0.5664 on 34 degrees of freedom
## Multiple R-Squared: 0.6441,  Adjusted R-squared: 0.6127 
## 
## Boot restarting based on 8 samples. Last fit:
## Convergence attained in 2 iterations (rel. change 3.648e-10)
```


### Workflow Summary for using data from GBIF to create a species-area relationship plot

1. Use `getKey(query, rank)` to find the unique identifying key associated with your taxon of interest.  
2. Use `getData(key, limit, geometry)` with the key found in Step 1 to access *limit* occurrence points from GBIF. There is a hard limit of 100,000 records for each call to `getData()`, as set by [*rgbif*](https://www.gbif.org/tool/81747/rgbif). The "geometry" parameter can be specified with a well-known text (WKT) representation of of geometry to restrict the geographic location of the returned occurrence records (see more about WKT below).
3. Use `findLand(occ, fillgaps)` with the dataframe obtained in Step 2 to figure out the names of landmasses using the occurrence record GPS points and the [*maps* R package](https://cran.r-project.org/web/packages/maps/index.html).  Setting the "fillgaps" parameter to `TRUE` will enable the use of [Photon API](https://photon.komoot.io/) to fill in any missing landmass names left by the *maps* R package.
4. Use `findAreas(occ, area_custom)` with the dataframe obtained in Step 3 to match the landmass names to a dataset that includes names of most islands on the planet and their areas. If the user would like to use a custom island area dataset instead of the built-in one, the "area_custom" parameter can be set to the name of the custom island area dataframe.
4a. If you'd like to only include occurrence records from islands, you can remove continental records by using `removeContinents(occ)` with the dataframe returned by `findAreas()`
5. Use `SARP(occ, npsi)` with the dataframe obtained in Step 4 to create a species-area relationship plot that reports information important to the associated regression. The "npsi" parameter indicates the maximum number of breakpoints the user would like to compare for model selection. The returned model and plot correspond with the best-fit model.

### Workflow summary for using data from GBIF and a user-provided phylogenetic tree to create a speciation-area relationship plot

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
