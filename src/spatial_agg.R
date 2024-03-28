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

agg_funcs <- c("mean", "sum", "count")

mappings_funcs <- list()
for (func in agg_funcs) {
  # Fill your list where each key is func and each value is the columns having the relevant aggregate function
  filt <- mapping %>%
    filter(str_detect(can_aggregate, func)) %>%
    pull(column)
  mappings_funcs[[func]] <- filt
  }

# Apply function
ngh_agg <- spatial_agg(
)

# Check the result
plot(out)
