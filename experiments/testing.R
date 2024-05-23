source("../src/spatial_agg.R")

# Loading packages for being able to manipulate and plot spatial data
library(sf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)

## Read data
# Reading in Adult R1 data
adult <- st_read("../tmp/data/R1/healsl_rd1_adult_v1.csv")

# Reading District Boundary file
dist <- st_read("../tmp/data/SL_bound/sl_dist_17_v2.geojson")

# Reading in GID boundary file
gid_r1 <- st_read("../tmp/data/SL_bound/sl_rd1_gid_v1.csv")

# Reading in ICD-10 code file
icd <- st_read("../tmp/data/ICD_10/icd10_cghr10_v1.csv")

# Added NA's to all empty cells in Adult data frame
adult <- adult %>% mutate_all(na_if,"") %>% mutate(final_icd_cod = case_when(!is.na(adj_icd_cod) ~ adj_icd_cod,  # Use adj_icd if it is not NA
                                                                             is.na(adj_icd_cod) & !is.na(p1_recon_icd_cod) & !is.na(p2_recon_icd_cod) ~ p1_recon_icd_cod,  # Use p1_recon_icd if adj_icd is NA and both p1_recon_icd and p2_recon_icd are not NA
                                                                             is.na(adj_icd_cod) & is.na(p1_recon_icd_cod) & is.na(p2_recon_icd_cod) ~ p1_icd_cod,  # Use p1_icd if both adj_icd and recon_icd are NA
                                                                             TRUE ~ NA_character_  # Default case, if none of the above conditions are met
)
) 

# Created new column for adult displaying final ICD-10 code cause of death


# Remove neonatal and child records from ICD codes
icd <- filter(icd, cghr10_age == "adult")
  
# Assign CGHR-10 title for corresponding record codes
adult <- left_join(adult, icd, by = setNames("icd10_code", "final_icd_cod")) 
  
# Join Adult R1 data with GID file
adult_gid <- merge(adult, gid_r1, by = "geoid")



## Converting data types
# Convert data type of illness duration column
adult_gid$adurillness_value <- as.numeric(adult_gid$adurillness_value)

# Convert data type of District ID column
adult_gid$gid_dist <- as.integer(adult_gid$gid_dist)

# Set mapping dataframe
mapping <- data.frame(
  column = c("cghr10_title", "adurillness_value"),
  can_aggregate = c("count,mode", "sum,median,mean,min,max,sd,var") 
)

# Testing out function 
adult_cod <- spatial_agg(gdf = adult_gid, 
                         gdf_agg = dist,
                         mapping = mapping,
                         gdf_id = "gid_dist", 
                         gdf_agg_id = "gid",
                         is_spatial_join = FALSE,
                         count_col = "deaths")

simple_choro_map <- 
  ggplot() + 
  geom_sf(data = adult_cod, aes(fill = adurillness_value_sum))

simple_choro_map