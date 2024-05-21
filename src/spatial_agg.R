# Loading packages for being able to manipulate and plot spatial data
library(sf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)

## Read data

# Reading in Adult R1 data
adult <- st_read("../tmp/Data/R1/healsl_rd1_adult_v1.csv")

# Reading District Boundary file
dist <- st_read("../tmp/Data/SL_bound/sl_dist_17_v2.geojson")

# Reading in GID boundary file
gid_r1 <- st_read("../tmp/Data/SL_bound/sl_rd1_gid_v1.csv")

# Join Adult R1 data with GID file
adult_gid <- merge(adult, gid_r1, by = "geoid")



## Converting data types

# Convert data type of illness duration column
adult_gid$adurillness_value <- as.numeric(adult_gid$adurillness_value)

# Convert data type of District ID column
adult_gid$gid_dist <- as.integer(adult_gid$gid_dist)


# Set Mappings dataframe
mappings <- data.frame(
  column = c("arespcod", "adurillness_value"),
  can_aggregate = c("count,mode", "sum,median,mean,min,max") 
)



## Creating Spatial Aggregation function
spatial_agg <- function(gdf, gdf_agg, gdf_join, gdf_agg_join, 
                        gdf_agg_id, mapping, is_spatial_join){
  
  # Perform joins
  if (is_spatial_join == TRUE){
    
    # Spatial join
    join_gdf <- gdf %>% 
      st_join(gdf, gdf_agg)
    
  } else {
    
    # Non spatial join
    join_gdf <-
      left_join(gdf, gdf_agg, by = setNames(gdf_agg_join, gdf_join))
  }
  
  
  # Group the joined dataframe
  group_gdf <- join_gdf %>% group_by(.data[[gdf_agg_id]])
  
  # List of aggregation functions available to use
  agg_funcs <- c("mean", "sum", "mode")
 
  # List of mappings functions that match with the joined gdf
  mappings_funcs <- list()

  #List for aggregation results
  agg_list <- list()
  
  
  # Performing aggregation for columns in mappings
  for (func_name in agg_funcs) {
    
    if (func_name %in% c("mode", "mean", "sum")){
      
      # Retrieve aggregation function connected to corresponding column
      mappings_funcs[[func_name]] <- mappings %>%
        filter(str_detect(can_aggregate, func_name)) %>%
        pull(column)
      
      # Convert function names to functions
      func <- get(func_name)
      
      # Apply functions
      if (func_name == "mode") {
        
        # Mode does not remove NA's
        agg_list[[func_name]] <- group_gdf %>%
          summarise_at(
            mappings_funcs[[func_name]],
            func
          )
        
        
      } else {
        
        # Other functions remove NA's
        agg_list[[func_name]] <- group_gdf %>%
          summarise_at(
            mappings_funcs[[func_name]],
            func,
            na.rm = TRUE
          )
      }
  
      
      
    }
    
    # Rename aggregation results columns
    agg_list[[func_name]] <- agg_list[[func_name]] %>%
      rename_with(
        .fn = ~ paste0( ., "_", func_name),
        .cols = everything()
      )
  }
  
  # Combine aggregation results and renamed columns into singular vector
  agg_results <- bind_cols(agg_list)
  
  return(agg_results)
  
}

# Testing out function 
adult_cod <- spatial_agg(gdf = adult_gid, 
                         gdf_agg = dist, 
                         gdf_join = "gid_dist", 
                         gdf_agg_join = "gid", 
                         gdf_agg_id = "gid_dist",
                         mapping = mappings,
                         is_spatial_join = FALSE)


# Joining function results back to district boundaries
dist_adult_cod <- left_join(dist, adult_cod, by = setNames("gid", "gid_dist_mean"))