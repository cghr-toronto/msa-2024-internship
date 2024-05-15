library(sf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)

## Read data
adult <- st_read("tmp/Data/R1/healsl_rd1_adult_v1.csv")

dist <- st_read("tmp/Data/SL_bound/sl_dist_17_v2.geojson")

gid_r1 <- st_read("tmp/Data/SL_bound/sl_rd1_gid_v1.csv")

adult_gid <- merge(adult, gid_r1, by = "geoid")

adult_gid$adurillness_value <- as.numeric(adult_gid$adurillness_value)

# Creating spatial_agg function
spatial_agg <- function(gdf, gdf_agg, gdf_join, gdf_agg_join, gdf_agg_id, mappings, is_spatial_join){

  if (is_spatial_join == TRUE){
  
  # Join gdf and gdf_agg
  sjoin_gdf = st_join(gdf, gdf_agg)
  
  # Group the spatial joins
  grouped_sjoin = group_by(sjoin_gdf, gdf_agg_id) 
  
  } else {
  
    non_sjoin_gdf <- left_join(gdf, gdf_agg, by = c(gdf_join, gdf_agg_join))
    
    grouped_non_sjoin = group_by(sjoin_gdf, gdf_agg_id) 
  }

}


mappings <- data.frame(
  column = c("arespcod", "adurillness_value"),
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
    sum_func[[func_name]] <- summarise_at(adult_gid, mappings_funcs[[func_name]], func, na.rm = TRUE) %>%
      rename_with(
        .fn = ~ paste0(func_name, "_", .),
        .cols = everything()
      )
    
  }
}

agg_results <- bind_cols(sum_func)