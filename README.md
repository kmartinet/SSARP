# SSARP - IN BETA

SSARP (Species-/Speciation-Area Relationship Projector) is an R package that provides a suite of functions to help users **create speciation- (later update) and species-area relationships for island-dwelling taxa** using occurrence data from GBIF (Global Biodiversity Information Facility) accessed via GBIF's API or the user's own occurrence data.

### Basic Workflow - Using data from GBIF to create a species-area relationship plot

1. Use **getKey(query, rank)** to find the unique identifying key associated with your taxon of interest.  
2. Use **getData(key, limit)** with the key found in Step 1 to access *limit* occurrence points from GBIF. There is a hard limit of 100,000 records for each call to **getData()**, as set by [*rgbif*](https://www.gbif.org/tool/81747/rgbif).  
3. Use **findLand(occ)** with the dataframe obtained in Step 2 to figure out the names of landmasses using the occurrence record GPS points and the [*maps* R package](https://cran.r-project.org/web/packages/maps/index.html).  
4. Use **findAreas(occ)** with the dataframe obtained in Step 3 to match the landmass names to a dataset that includes names of most islands on the planet and their areas.
5. Use **SARP(areas, npsi)** with the dataframe obtained in Step 4 to create a species-area relationship plot that reports information important to the associated regression. The *npsi* parameter indicates how many breakpoints the user would like the segmented regression to include. A near-future update of this package will include automatic model selection for the optimal npsi when the user is unsure of how many breakpoints are appropriate for their occurrence dataset.
