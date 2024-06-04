source("../src/spatial_agg.R")

# Loading packages for being able to manipulate and plot spatial data
library(sf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(ggspatial)
library(tmap)
library(prettymapr)

## Read data
# Reading in Adult Round 1 and Round 2 data
adult_r1 <- st_read("../tmp/data/healsl_rd1_adult_v1.csv")
adult_r2 <- st_read("../tmp/data/healsl_rd2_adult_v1.csv")

# Reading District Boundary file
dist <- st_read("../tmp/data/sl_dist_17_v2.geojson")

# Reading in GID r1 and r2 boundary file
gid_r1 <- st_read("../tmp/data/sl_rd1_gid_v1.csv")
gid_r2 <- st_read("../tmp/data/sl_rd2_gid_v1.csv")

# Reading in ICD-10 code file
icd <- st_read("../tmp/data/icd10_cghr10_v1.csv")
  
# Join Adult datasets with GID files
adult_r1_gid <- left_join(adult_r1, gid_r1, by = "geoid")
adult_r2_gid <- left_join(adult_r2, gid_r2, by = "geoid")

# Combine r1 and r2 adult data
adult <- bind_rows(adult_r1_gid, adult_r2_gid)

# Created new column for adult displaying final ICD-10 code cause of death
adult <- adult %>% mutate_all(na_if,"") %>% 
    mutate(final_icd_cod = case_when(!is.na(adj_icd_cod) ~ adj_icd_cod,  # Use adj_icd if it is not NA
                                     is.na(adj_icd_cod) & !is.na(p1_recon_icd_cod) & !is.na(p2_recon_icd_cod) ~ p1_recon_icd_cod,  # Use p1_recon_icd if adj_icd is NA and both p1_recon_icd and p2_recon_icd are not NA
                                     is.na(adj_icd_cod) & is.na(p1_recon_icd_cod) & is.na(p2_recon_icd_cod) ~ p1_icd_cod,  # Use p1_icd if both adj_icd and recon_icd are NA
                                     TRUE ~ NA_character_  # Default case, if none of the above conditions are met
    )
    ) 

# Remove neonatal and child records from ICD codes
icd <- filter(icd, cghr10_age == "adult")

# Assign CGHR-10 title for corresponding record codes
adult <- left_join(adult, icd, by = setNames("icd10_code", "final_icd_cod")) 

# Filter to only malaria
adult_malaria <- adult %>% filter(cghr10_title == "Malaria")

## Converting data types examples
# Convert data type of illness duration column
# adult_gid$adurillness_value <- as.numeric(adult_gid$adurillness_value)

# Convert data type of District ID column
adult_malaria$gid_dist <- as.integer(adult_malaria$gid_dist)

# Set mapping dataframe
mapping <- data.frame(
  column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10"),
  can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count") 
)

# Testing out function with adult malaria
adult_cod <- spatial_agg(gdf = dist,
                         agg = adult_malaria,
                         mapping = mapping,
                         gdf_id = "gid", 
                         agg_id = "gid_dist",
                         is_spatial_join = FALSE,
                         count_col = "deaths")

# Remove geometry from adult_cod
adult_cod_without_geometry <- adult_cod  %>%
    as_tibble() %>%
    select(-geometry, -deaths)

# Creating spatial symptom count
result <- adult_cod_without_geometry %>%
    pivot_longer( cols = matches("^symp\\d+_"), # Matches columns starting with "symp" followed by dig
        names_to = "symptom", # New column to store the symptom names
        values_to = "count" # New column to store the counts
    ) %>% mutate(symptom = gsub("^symp\\d+_|_count$","", symptom)) %>% # Remove prefix and suff
    group_by(gid, symptom) %>% # Group by gid and sympt
    summarize(total_count = sum(count) # Summarize the counts f
    ) %>% pivot_wider( names_from = symptom, # Pivot symptom column to wide format
                 values_from = total_count, # Values to be filled in the wide format
                 values_fill = 0 # Fill any missing values with 0
    )

# Join geometry to new spatial table
spatial <- result %>%
    left_join(adult_cod %>% select(gid, geometry, deaths), by = "gid")

# Print the wide format
cat("\nWide format:\n")
print(spatial)

# Convert spatial to an sf and reproject crs
spatial <- spatial %>% st_as_sf(sf_column_name = "geometry") %>% st_transform(32628)

# Creating non-spatial table of symptom and causes of death
non_spatial <- pivot_longer(adult, cols = starts_with("symp"), # Matches columns starting with "symp" followed by dig
        names_to = "symptom", # New column to store the symptom names
        values_to = "value" # New column to store the counts
    ) %>% group_by(cghr10_title, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(cghr10_title, value) %>%
    pivot_wider(
        names_from = value,   # The values in the 'value' column will become column names
        values_from = count,  # The values in the 'count' column will fill the new columns
        values_fill = list(count = 0)  # Fill missing values with 0
    )

# Creating count for deaths per cause in non-spatial
death_count <- adult %>% count(cghr10_title, sort = TRUE, name = "deaths")
non_spatial <- non_spatial %>% left_join(death_count, by = "cghr10_title")

jaundice <- ggplot() +
    geom_sf(data = spatial, aes(geometry = geometry, fill=((yellowEyes/deaths) * 100))) +
    guides(fill = guide_legend(title = "Cases per 100 deaths")) +
    scale_fill_gradient(low="lightblue", high="darkblue", breaks = c(0,2,4,6,8)) +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Adult Malaria Cases with Jaundice")

coughing <- ggplot() +
    geom_sf(data = spatial, aes(geometry = geometry, fill=((cough/deaths) * 100))) +
    guides(fill = guide_legend(title = "Cases per 100 deaths")) +
    scale_fill_gradient(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Adult Malaria Cases with Coughing")

vomit <- ggplot() +
    geom_sf(data = spatial, aes(geometry = geometry, fill=((vomit/deaths) * 100))) +
    guides(fill = guide_legend(title = "Cases per 100 deaths")) +
    scale_fill_gradient(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Adult Malaria Cases with Vomit")

bp <- ggplot() +
    geom_sf(data = spatial, aes(geometry = geometry, fill=((breathingProblem/deaths) * 100))) +
    guides(fill = guide_legend(title = "Cases per 100 deaths")) +
    scale_fill_gradient(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Adult Malaria Cases with Breathing Problems")

ap <- ggplot() +
    geom_sf(data = spatial, aes(geometry = geometry, fill=((abdominalProblem/deaths) * 100))) +
    guides(fill = guide_legend(title = "Cases per 100 deaths")) +
    scale_fill_gradient(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Adult Malaria Cases with Abdominal Problems")

jaundice
coughing
vomit
bp
ap
