library(sf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)

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

mappings <- data.frame(
  column = c("AREA_SHORT_CODE", "AREA_ID"),
  can_aggregate = c("count,mode", "sum,median,mean,min,max") 
)

agg_funcs <- c("mean", "sum", "mode")

mappings_funcs <- list()

sum_func <- list()

for (func_name in agg_funcs) {
  
  if (func_name %in% c("mode", "mean", "sum")){
    
    # Get cols
    mappings_funcs[[func_name]] <- mappings %>%
      filter(str_detect(can_aggregate, func_name)) %>%
      pull(column)
    
    # Get
    func <- get(func_name)
    
    # Apply
    sum_func[[func_name]] <- summarise_at(ngh, mappings_funcs[[func_name]], func) %>%
      rename_with(
        .fn = ~ paste0(func_name, "_", .),
        .cols = everything()
      )
    
  }
}

combined_funcs <- bind_cols(sum_func)