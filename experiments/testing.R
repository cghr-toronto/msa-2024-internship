source("../src/spatial_agg.R")

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
  can_aggregate = c("count,mode", "sum,median,mean,min,max,sd,var") 
)

# Testing out function 
adult_cod <- spatial_agg(gdf = adult_gid, 
                         gdf_agg = dist, 
                         gdf_join = "gid_dist", 
                         gdf_agg_join = "gid", 
                         gdf_agg_id = "gid_dist",
                         mapping = mappings,
                         mapping_col = "column", 
                         mapping_agg = "can_aggregate",
                         is_spatial_join = FALSE)

simple_choro_map <- 
  ggplot() + 
  geom_sf(data = adult_cod, aes(fill = adurillness_value_sum))

simple_choro_map
