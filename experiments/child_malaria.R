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
library(patchwork)

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

# Get unique district names
uniq_dname <- unique(dist$distname)
uniq_dcod <- unique(child$district_cod)

# See distname and district_cod
cat(
    "Before Fix\n----------\n\ndistname:\n",
    paste0(uniq_dname, collapse = "\n "),
    "\n\ndistrict_cod:\n",
    paste0(uniq_dcod, collapse = "\n "),
    "\n\n"
)

# Find all distname not in district_cod
dname_nin_dcod <- uniq_dname[!uniq_dname %in% uniq_dcod]

# Find all district_cod not in distname
dcod_nin_dname <- uniq_dcod[!uniq_dcod %in% uniq_dname]

# See conflicting distname and district_cod
cat(
    "\nBefore Fix\n----------\n\ndistname not in district_cod:\n",
    paste0(dname_nin_dcod, collapse = "\n "),
    "\n\ndistrict_cod not in distname:\n",
    paste0(dcod_nin_dname, collapse = "\n "),
    "\n"
)

# Match conflicts (EDIT AS NEEDED)
# Probably a good idea to place this before joining by distname
# THIS CODE IS PLACED IN YOUR MAIN CODE FILE TO FIX dist
dist <- dist %>%
    mutate(
        distname = case_when(
            distname == "Western Area Rur" ~ "Western Area Rural",
            distname == "Western Area Urb" ~ "Western Area Urban",
            .default = distname)
    )

# Get unique district names
uniq_dname <- unique(dist$distname)
uniq_dcod <- unique(child$district_cod)

# See distname and district_cod
cat(
    "After Fix\n---------\n\ndistname:\n",
    paste0(uniq_dname, collapse = "\n "),
    "\n\ndistrict_cod:\n",
    paste0(uniq_dcod, collapse = "\n "),
    "\n"
)

# Find all distname not in district_cod
dname_nin_dcod <- uniq_dname[!uniq_dname %in% uniq_dcod]

# Find all district_cod not in distname
dcod_nin_dname <- uniq_dcod[!uniq_dcod %in% uniq_dname]

# See conflicting distname and district_cod
cat(
    "\nAfter Fix\n---------\n\ndistname not in district_cod:\n",
    paste0(dname_nin_dcod, collapse = "\n "),
    "\n\ndistrict_cod not in distname:\n",
    paste0(dcod_nin_dname, collapse = "\n "),
    "\n"
)

# Corrected district_cod to correct values
child <- child %>%
    mutate(district_cod = ifelse(is.na(district_cod) | district_cod == "",
                             case_when(
                                 gid_dist == 1 ~ "Kailahun",
                                 gid_dist == 2 ~ "Kenema",
                                 gid_dist == 3 ~ "Bombali",
                                 gid_dist == 4 ~ "Kono",
                                 gid_dist == 5 ~ "Tonkolili",
                                 gid_dist == 6 ~ "Bo",
                                 gid_dist == 7 ~ "Karene",
                                 gid_dist == 8 ~ "Kambia",
                                 gid_dist == 9 ~ "Falaba",
                                 gid_dist == 10 ~ "Koinadugu",
                                 gid_dist == 11 ~ "Port Loko",
                                 gid_dist == 12 ~ "Bonthe",
                                 gid_dist == 13 ~ "Pujehun",
                                 gid_dist == 14 ~ "Moyamba",
                                 gid_dist == 15 ~ "Western Area Rural",
                                 gid_dist == 16 ~ "Western Area Urban",
                                 TRUE ~ district_cod
                             ),
                             district_cod))

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
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10",
               "symp11", "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count", "count", "count") 
)

# Testing out function with child malaria
male_child_malaria <- spatial_agg(gdf = dist,
                                        agg = male_child,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "malaria_deaths")

female_child_malaria <- spatial_agg(gdf = dist,
                                          agg = female_child,
                                          mapping = mapping,
                                          gdf_id = "distname", 
                                          agg_id = "district_cod",
                                          is_spatial_join = FALSE,
                                          count_col = "malaria_deaths")

child_agg <- spatial_agg(gdf = dist,
                         agg = child,
                         mapping = mapping,
                         gdf_id = "distname", 
                         agg_id = "district_cod",
                         is_spatial_join = FALSE,
                         count_col = "all_deaths")

# Defining symptoms to be plotted
child_symptoms <- c("fever", "convulsion", "difficultyBreathing", "cough", "vomit",
                    "headache", "yellowEyes")

# Running symptom_rate for each sex group
cm_symptom <- symptom_rate(age_sex_agg = male_child_malaria,
                            all_agg = child_agg, deaths = "malaria_deaths",
                            symptoms = child_symptoms)
cf_symptom <- symptom_rate(age_sex_agg = female_child_malaria,
                            all_agg = child_agg, deaths = "malaria_deaths",
                            symptoms = child_symptoms)

# Creating non-spatial table of symptom and causes of death
non_spatial_children <- pivot_longer(child, cols = starts_with("symp"), # Matches columns starting with "symp" followed by dig
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

colnames(non_spatial_children)[colnames(non_spatial_children) == "cghr10_title"] <- "cause_of_death"

# Creating maps for each age group
cm_plot <- create_plots(cm_symptom, "Child Male Malaria Symptoms")
cf_plot <- create_plots(cf_symptom, "Child Female Malaria Symptoms")

# Viewing plots for each map series
cm_plot
cf_plot

# Export as pdf
cm_pdf <- pdf_print(cm_plot, "fig-cm-malaria-maps")
cf_pdf <- pdf_print(cf_plot, "fig-cf-malaria-maps")

# Creating heat map with non-spatial table
heat <- pivot_longer(non_spatial_children, cols = -cause_of_death,
                     names_to = "symptoms",
                     values_to = "rates") %>%
    filter(cause_of_death != "NA" & symptoms != "NA" & symptoms != "deaths")

heat_map_children <- ggplot(heat, aes(symptoms, cause_of_death)) +
    geom_tile(aes(fill = rates)) +
    geom_text(aes(label = round(rates, 1))) +
    scale_fill_gradient(low = "white", high = "red") +
    theme(axis.text.x = element_text(size = 3))

# Viewing plot of heat map
heat_map_children

# Exporting heat map as pdf
hm_children <- pdf_print(heat_map_children, "Child Heatmap")