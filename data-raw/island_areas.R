# Data import

island_areas <- read.csv("data-raw/island_areas.csv")

usethis::use_data(island_areas, overwrite = TRUE)
