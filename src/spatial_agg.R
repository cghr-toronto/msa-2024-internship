library(sf)
library(tidyverse)
library(ggplot2)

## Read data
# Reading in the Neighbourhoods GeoJson File and reprojecting
ngh <- st_read("data/Neighbourhoods.geojson") %>% st_transform(3857)

# Reading in the Neighbourhood Improvement Areas GeoJson File and reprojecting
ngh_imp <- st_read("data/Neighbourhood Improvement Areas.geojson") %>% st_transform(3857)

# Set mappings
mappings <- list(
  list(
    column = "cause",
    how = c("count", "mode", "mean", "sum"),
    is_cat = TRUE
  )
)

# Apply function
out <- spatial_agg(
  gdf,
  gdf_agg,
  id_col = "id",
  mappings = mappings
)

# Check the result
plot(out)
