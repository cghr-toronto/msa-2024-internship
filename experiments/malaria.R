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

# Convert data type of District ID column
adult$gid_dist <- as.integer(adult$gid_dist)

# Creating age ranges for adults
young_adult_age <- c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39")
older_adult_age <- c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

# Creating filters for young adults by sex, age, and malaria
young_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age & cghr10_title == "Malaria")
young_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age & cghr10_title == "Malaria")

# Creating filters for older adults by sex, age, and malaria
older_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age & cghr10_title == "Malaria")
older_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age & cghr10_title == "Malaria")

# Dataframe without malaria deaths
adult_non_malaria <- adult %>% filter(cghr10_title != "Malaria")

# Set mapping dataframe
mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count") 
)

# Testing out function with adult malaria
young_male_adult_malaria <- spatial_agg(gdf = dist,
                                 agg = young_male_adult,
                                 mapping = mapping,
                                 gdf_id = "gid", 
                                 agg_id = "gid_dist",
                                 is_spatial_join = FALSE,
                                 count_col = "malaria_deaths")

young_female_adult_malaria <- spatial_agg(gdf = dist,
                                        agg = young_female_adult,
                                        mapping = mapping,
                                        gdf_id = "gid", 
                                        agg_id = "gid_dist",
                                        is_spatial_join = FALSE,
                                        count_col = "malaria_deaths")

older_male_adult_malaria <- spatial_agg(gdf = dist,
                                        agg = older_male_adult,
                                        mapping = mapping,
                                        gdf_id = "gid", 
                                        agg_id = "gid_dist",
                                        is_spatial_join = FALSE,
                                        count_col = "malaria_deaths")

older_female_adult_malaria <- spatial_agg(gdf = dist,
                                        agg = older_female_adult,
                                        mapping = mapping,
                                        gdf_id = "gid", 
                                        agg_id = "gid_dist",
                                        is_spatial_join = FALSE,
                                        count_col = "malaria_deaths")

adult_agg <- spatial_agg(gdf = dist,
                         agg = adult,
                         mapping = mapping,
                         gdf_id = "gid", 
                         agg_id = "gid_dist",
                         is_spatial_join = FALSE,
                         count_col = "all_deaths")

# Function for creating rates for aggregated results
symptom_rate <- function(
        adult_malaria_agg,
        adult_agg){

# Remove geometry from aggregated dataframe
adult_malaria_without_geometry <- adult_malaria_agg  %>%
    as_tibble() %>%
    select(-geometry, -malaria_deaths, -distname)

# Creating spatial symptom count
result <- adult_malaria_without_geometry %>%
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
    left_join(adult_malaria_agg %>% select(gid, geometry, malaria_deaths, distname), by = "gid")

# Add all deaths to malaria table
spatial$all_deaths <- adult_agg$all_deaths

# Create rate columns for malaria symptoms
spatial$yellowEyes_rate <- (spatial$yellowEyes/spatial$all_deaths) * 1000 
spatial$cough_rate <- (spatial$cough/spatial$all_deaths) * 1000
spatial$vomit_rate <- (spatial$vomit/spatial$all_deaths) * 1000
spatial$breathingProblem_rate <- (spatial$breathingProblem/spatial$all_deaths) * 1000
spatial$abdominalProblem_rate <- (spatial$abdominalProblem/spatial$all_deaths) * 1000

# Round to 2 decimal places
spatial <- spatial %>% mutate(yellowEyes_rate = round(yellowEyes_rate, 2))
spatial <- spatial %>% mutate(cough_rate = round(cough_rate, 2))
spatial <- spatial %>% mutate(vomit_rate = round(vomit_rate, 2))
spatial <- spatial %>% mutate(breathingProblem_rate = round(breathingProblem_rate, 2))
spatial <- spatial %>% mutate(abdominalProblem_rate = round(abdominalProblem_rate, 2))

# Print the wide format
cat("\nWide format:\n")
print(spatial)

# Convert spatial to an sf and reproject crs
out <- spatial %>% st_as_sf(sf_column_name = "geometry") %>% st_transform(32628)

return(out)

}

yam_symptom <- symptom_rate(adult_malaria_agg = young_male_adult_malaria,
                            adult_agg = adult_agg)
yaf_symptom <- symptom_rate(adult_malaria_agg = young_female_adult_malaria,
                            adult_agg = adult_agg)
oam_symptom <- symptom_rate(adult_malaria_agg = older_male_adult_malaria,
                            adult_agg = adult_agg)
oaf_symptom <- symptom_rate(adult_malaria_agg = older_female_adult_malaria,
                            adult_agg = adult_agg)

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

jaundice <- ggplot(yam_symptom) +
    geom_sf(aes(geometry = geometry, fill=(yellowEyes_rate))) +
    guides(fill = guide_legend(title = "Cases per 1000 deaths")) +
    scale_fill_continuous(low="lightblue", high="darkblue", breaks = c(0,2,4,6,8,10)) +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Young Adult Male Malaria Cases with Jaundice") +
    geom_sf_label(aes(label = yellowEyes_rate), size = 2.5) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank())

coughing <- ggplot(yam_symptom) +
    geom_sf(aes(geometry = geometry, fill=(cough_rate))) +
    guides(fill = guide_legend(title = "Cases per 1000 deaths")) +
    scale_fill_continuous(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Young Adult Male Malaria Cases with Coughing")+
    geom_sf_label(aes(label = cough_rate), size = 2.5) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank())


vomit <- ggplot(yam_symptom) +
    geom_sf(aes(geometry = geometry, fill=(vomit_rate))) +
    guides(fill = guide_legend(title = "Cases per 1000 deaths")) +
    scale_fill_continuous(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Young Adult Male Malaria Cases with Vomit")+
    geom_sf_label(aes(label = vomit_rate), size = 2.5) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank())


bp <- ggplot(yam_symptom) +
    geom_sf(aes(geometry = geometry, fill=(breathingProblem_rate))) +
    guides(fill = guide_legend(title = "Cases per 1000 deaths")) +
    scale_fill_continuous(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Young Adult Male Malaria Cases with Breathing Problems")+
    geom_sf_label(aes(label = breathingProblem_rate), size = 2.5) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank())


ap <- ggplot(yam_symptom) +
    geom_sf(aes(geometry = geometry, fill=(abdominalProblem_rate))) +
    guides(fill = guide_legend(title = "Cases per 1000 deaths")) +
    scale_fill_continuous(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    labs(title = "Young Adult Male Malaria Cases with Abdominal Problems")+
    geom_sf_label(aes(label = abdominalProblem_rate), size = 2.5) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank())


jaundice
coughing
vomit
bp
ap
