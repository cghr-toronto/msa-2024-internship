library(sf)
library(tidyverse)
library(ggplot2)

## Read data
# Reading in the Neighbourhoods GeoJson File and reprojecting
ngh <- st_read("...experiments/data/Neighbourhoods.geojson") %>% st_transform(3857)

# Reading in the Neighbourhood Improvement Areas GeoJson File and reprojecting
ngh_imp <- st_read("...experiments/data/Neighbourhood Improvement Areas.geojson") %>% st_transform(3857)

# Creating spatial_agg function
spatial_agg <- function(gdf, gdf_agg, id_col, mappings){

  # Join gdf and gdf_agg
  sjoin_gdf = st_join(gdf, gdf_agg)
  
  # Group the spatial joins
  grouped_sjoin = group_by(sjoin_gdf, gdf_agg_id)
  
  # Set mappings
  mappings <- list(
    list(
      column = "cause",
      how = c("count", "mode", "mean", "sum"),
      is_cat = TRUE
   )
  )
  
}

# Apply function
out <- spatial_agg(
  gdf,
  gdf_agg,
  id_col = "id",
  mappings = mappings
)

# Check the result
plot(out)
