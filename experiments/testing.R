source("../src/spatial_agg.R")

# Loading packages for being able to manipulate and plot spatial data
library(sf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)

## Read data
# Reading in Adult Round 1 and Round 2 data
adult_r1 <- st_read("../tmp/data/healsl_rd1_adult_v1.csv")
adult_r2 <- st_read("../tmp/data/healsl_rd2_adult_v1.csv")

# Reading District Boundary file
dist <- st_read("../tmp/data/sl_dist_17_v2.geojson")

# Reading in GID boundary file
gid_r1 <- st_read("../tmp/data/sl_rd1_gid_v1.csv")

# Reading in ICD-10 code file
icd <- st_read("../tmp/data/icd10_cghr10_v1.csv")

# Combine r1 and r2 adult data
adult <- bind_rows(adult_r1, adult_r2)

# Created new column for adult displaying final ICD-10 code cause of death
adult <- adult %>% mutate_all(na_if,"") %>% mutate(final_icd_cod = case_when(!is.na(adj_icd_cod) ~ adj_icd_cod,  # Use adj_icd if it is not NA
                                                                             is.na(adj_icd_cod) & !is.na(p1_recon_icd_cod) & !is.na(p2_recon_icd_cod) ~ p1_recon_icd_cod,  # Use p1_recon_icd if adj_icd is NA and both p1_recon_icd and p2_recon_icd are not NA
                                                                             is.na(adj_icd_cod) & is.na(p1_recon_icd_cod) & is.na(p2_recon_icd_cod) ~ p1_icd_cod,  # Use p1_icd if both adj_icd and recon_icd are NA
                                                                             TRUE ~ NA_character_  # Default case, if none of the above conditions are met
)
) 

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
  column = c("symp1", "symp2", "symp3", "symp4", "symp5"),
  can_aggregate = c("count", "count", "count", "count", "count") 
)

# Testing out function 
adult_cod <- spatial_agg(gdf = dist,
                         agg = adult_gid,
                         mapping = mapping,
                         gdf_id = "gid", 
                         agg_id = "gid_dist",
                         is_spatial_join = FALSE,
                         count_col = "deaths")

adult_cod_without_geometry <- adult_cod  %>%
    as_tibble() %>%
    select(-geometry)

result <- adult_cod_without_geometry %>%
    pivot_longer( cols = matches(
        "^symp\\d+_"), # Matches columns starting with "symp" followed by dig
        names_to = "symptom", # New column to store the symptom names
        values_to = "count" # New column to store the counts
    ) %>% mutate(symptom = gsub("^symp\\d+_|_count$","", symptom)) %>% # Remove prefix and suff
    group_by(gid, symptom) %>% # Group by gid and sympt
    summarize(total_count = sum(count)) # Summarize the counts f
    
wide <- result %>% pivot_wider( names_from = symptom, # Pivot symptom column to wide format
                 values_from = total_count, # Values to be filled in the wide format
                 values_fill = 0 # Fill any missing values with 0
    )


final_wide <- wide %>%
    left_join(adult_cod %>% select(gid, geometry), by = "gid")

# Print the wide format
cat("\nWide format:\n")
print(result)

# Print the long format
cat("\nLong format:\n")
print(final_wide)
