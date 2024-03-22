library(sf)
library(tidyverse)
library(ggplot2)

## Read data
# Reading in the Neighbourhoods GeoJson File and reprojecting
ngh <- st_read("experiments/data/Neighbourhoods.geojson") %>% st_transform(3857)

# Reading in the Neighbourhood Improvement Areas GeoJson File and reprojecting
ngh_imp <- st_read("experiments/data/Neighbourhood Improvement Areas.geojson") %>% st_transform(3857)

# Creating spatial_agg function
spatial_agg <- function(gdf, gdf_agg, gdf_agg_id, mappings){

  # Join gdf and gdf_agg
  sjoin_gdf = st_join(gdf, gdf_agg)
  
  # Group the spatial joins
  grouped_sjoin = group_by(sjoin_gdf, gdf_agg_id) 

}

mapping <- data.frame(
  column = c("cause", "age"),
  can_aggregate = c("count,mode", "sum,median,mean,min,max") 
)

filt <- mapping %>%
  filter(can_aggregate == "mean") %>%
  pull(column)

View(mapping) # before
View(filt) # after

# Apply function
ngh_agg <- spatial_agg(
  ngh,
  ngh_imp,
  ngh_imp_id = "X_id",
  mappings = mappings
)

# Check the result
plot(out)
