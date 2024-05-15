library(sf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)

## Read data
adult <- st_read("../tmp/data/R1/healsl_rd1_adult_v1.csv")

dist <- st_read("../tmp/data/SL_bound/sl_dist_17_v2.geojson")

gid_r1 <- st_read("../tmp/data/SL_bound/sl_rd1_gid_v1.csv")

adult_gid <- merge(adult, gid_r1, by = "geoid")

# Convert data types
adult_gid$adurillness_value <- as.numeric(adult_gid$adurillness_value)

# Creating spatial_agg function
spatial_agg <- function(gdf, gdf_agg, gdf_join, gdf_agg_join, gdf_agg_id, mapping, mapping_agg, mapping_col, is_spatial_join){
  
  # Perform joins
  if (is_spatial_join == TRUE){
    
    # Spatial join
    join_gdf <- gdf %>% 
      st_join(gdf, gdf_agg)
    
  } else {
    
    # Non spatial join
    join_gdf <- gdf %>%
      left_join(gdf_agg, by = c(gdf_join, gdf_agg_join))
  }
  
  # Group the joins
  group_gdf = group_by(join_gdf, gdf_agg_id)
  
  # Aggregating the grouped gdfs
  group_gdf_agg <- group_gdf %>% mappings$mapping_agg(mappings$mappinh_col)

}


mappings <- data.frame(
  column = c("arespcod", "adurillness_value"),
  can_aggregate = c("count,mode", "sum,median,mean,min,max") 
)

agg_funcs <- c("mean", "sum", "mode")

mappings_funcs <- list()

gdf_agg <- list()

for (func_name in agg_funcs) {
  
  if (func_name %in% c("mode", "mean", "sum")){
    
    # Get cols
    mappings_funcs[[func_name]] <- mappings %>%
      filter(str_detect(can_aggregate, func_name)) %>%
      pull(column)
    
    # Get
    func <- get(func_name)
    
    # Apply func
    if (func_name == "mode") {
      
      # Mode does not remove nas
      gdf_agg[[func_name]] <- adult_gid %>%
        summarise_at(
          mappings_funcs[[func_name]],
          func
        )
      
    } else {
      
      # Other funcs remove nas
      gdf_agg[[func_name]] <- adult_gid %>%
        summarise_at(
          mappings_funcs[[func_name]],
          func,
          na.rm = TRUE
        )
    }
    
    # Rename
    gdf_agg[[func_name]] <- gdf_agg[[func_name]] %>%
      rename_with(
        .fn = ~ paste0(func_name, "_", .),
        .cols = everything()
      )
    
  }
}

#combine columns from for loop
agg_results <- bind_cols(gdf_agg)