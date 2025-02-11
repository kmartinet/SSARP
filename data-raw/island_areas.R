# Data import

island_areas <- read.csv("data-raw/island_areas.csv")

# There are non-ASCII characters in the island names, so convert them to c99 style Unicode
island_areas$Name <- iconv(island_areas$Name, to='ASCII', sub = "c99")

# Export data
usethis::use_data(island_areas, overwrite = TRUE)
