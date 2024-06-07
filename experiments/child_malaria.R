source("../src/spatial_agg.R")
source("../experiments/adult_malaria.R")

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
# Reading in Child Round 1 and Round 2 data
child_r1 <- st_read("../tmp/data/healsl_rd1_child_v1.csv")
child_r2 <- st_read("../tmp/data/healsl_rd2_child_v1.csv")

# Reading District Boundary file
dist <- st_read("../tmp/data/sl_dist_17_v2.geojson")

# Reading in GID r1 and r2 boundary file
gid_r1 <- st_read("../tmp/data/sl_rd1_gid_v1.csv")
gid_r2 <- st_read("../tmp/data/sl_rd2_gid_v1.csv")

# Reading in ICD-10 code file
icd <- st_read("../tmp/data/icd10_cghr10_v1.csv")

# Join child datasets with GID files
child_r1_gid <- left_join(child_r1, gid_r1, by = "geoid")
child_r2_gid <- left_join(child_r2, gid_r2, by = "geoid")

# Combine r1 and r2 child data
child <- bind_rows(child_r1_gid, child_r2_gid)

# Created new column for child displaying final ICD-10 code cause of death
child <- child %>% mutate_all(na_if,"") %>% 
    mutate(final_icd = case_when(!is.na(adj_icd) ~ adj_icd,  # Use adj_icd if it is not NA
                                     is.na(adj_icd) & !is.na(p1_recon_icd) & !is.na(p2_recon_icd) ~ p1_recon_icd,  # Use p1_recon_icd if adj_icd is NA and both p1_recon_icd and p2_recon_icd are not NA
                                     is.na(adj_icd) & is.na(p1_recon_icd) & is.na(p2_recon_icd) ~ p1_icd,  # Use p1_icd if both adj_icd and recon_icd are NA
                                     TRUE ~ NA_character_  # Default case, if none of the above conditions are met
    )
    ) 

# Remove neonatal and child records from ICD codes
icd <- filter(icd, cghr10_age == "child")

# Assign CGHR-10 title for corresponding record codes
child <- left_join(child, icd, by = setNames("icd10_code", "final_icd"))

# Convert data type of District ID column
child$gid_dist <- as.integer(child$gid_dist)

# Creating filters for young childs by sex, age, and malaria
male_child <- child %>% filter(sex_death == "Male" & cghr10_title == "Malaria")
female_child <- child %>% filter(sex_death == "Female" & cghr10_title == "Malaria")

# Dataframe without malaria deaths
child_non_malaria <- child %>% filter(cghr10_title != "Malaria")

# Set mapping dataframe
mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count") 
)

# Testing out function with child malaria
male_child_malaria <- spatial_agg(gdf = dist,
                                        agg = male_child,
                                        mapping = mapping,
                                        gdf_id = "gid", 
                                        agg_id = "gid_dist",
                                        is_spatial_join = FALSE,
                                        count_col = "malaria_deaths")

female_child_malaria <- spatial_agg(gdf = dist,
                                          agg = female_child,
                                          mapping = mapping,
                                          gdf_id = "gid", 
                                          agg_id = "gid_dist",
                                          is_spatial_join = FALSE,
                                          count_col = "malaria_deaths")

child_agg <- spatial_agg(gdf = dist,
                         agg = child,
                         mapping = mapping,
                         gdf_id = "gid", 
                         agg_id = "gid_dist",
                         is_spatial_join = FALSE,
                         count_col = "all_deaths")

# Running symptom_rate for each sex group
cm_symptom <- symptom_rate(malaria_agg = male_child_malaria,
                            agg = child_agg)
cf_symptom <- symptom_rate(malaria_agg = female_child_malaria,
                            agg = child_agg)

# Creating non-spatial table of symptom and causes of death
non_spatial <- pivot_longer(child, cols = starts_with("symp"), # Matches columns starting with "symp" followed by dig
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
death_count <- child %>% count(cghr10_title, sort = TRUE, name = "deaths")
non_spatial <- non_spatial %>% left_join(death_count, by = "cghr10_title")

# Creating maps for each age group
cm_plot <- ggplot(cm_symptom) +
    geom_sf(aes(fill=(rates))) +
    guides(fill = guide_legend(title = "Cases per 1000 deaths")) +
    scale_fill_continuous(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Male Child Malaria Symptoms") +
    geom_sf_label(aes(label = rates), size = 2.5) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank()) +
    facet_wrap(~ symptoms)

cf_plot <- ggplot(cf_symptom) +
    geom_sf(aes(fill=(rates))) +
    guides(fill = guide_legend(title = "Cases per 1000 deaths")) +
    scale_fill_continuous(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Female Child Malaria Symptoms")+
    geom_sf_label(aes(label = rates), size = 2.5) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank()) +
    facet_wrap(~ symptoms)

cm_plot
cf_plot