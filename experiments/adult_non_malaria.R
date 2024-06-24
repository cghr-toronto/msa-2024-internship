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

# Find matching columns
same_cols <- colnames(adult_r2_gid)
same_cols <- same_cols[same_cols %in% colnames(adult_r1_gid)]

# Ensure both data have same columns
adult_r1_gid <- adult_r1_gid %>% select(all_of(same_cols))
adult_r2_gid <- adult_r2_gid %>% select(all_of(same_cols))

# # Fix conflicting date type columns in round 1 and round 2 adult
# adult_r1_gid <- adult_r1_gid %>%
#     mutate(across(
#         .cols = starts_with("time") | ends_with("time") | ends_with("date"),
#         .fns = ~ na_if(., "")
#     )) %>% 
#     mutate(across(
#         .cols = starts_with("time") | ends_with("time") | ends_with("date"),
#         .fns = ~ ymd(.)
#     ))

# Combine r1 and r2 adult data
adult <- bind_rows(adult_r1_gid, adult_r2_gid)

# Get unique district names
uniq_dname <- unique(dist$distname)
uniq_dcod <- unique(adult$district_cod)

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
uniq_dcod <- unique(adult$district_cod)

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
adult <- adult %>%
    mutate(district_cod = ifelse(gid_dist == 7, "Karene",
                                 ifelse(gid_dist == 9, "Falaba", district_cod))) %>%
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

# Creating age ranges for adults
young_adult_age <- c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39")
older_adult_age <- c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

# Creating filters for young adults by sex, age, and malaria
young_male_adult_non_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age & cghr10_title != "Malaria")
young_female_adult_non_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age & cghr10_title != "Malaria")
young_adult_non_malaria <- adult %>% filter(death_age_group %in% young_adult_age & cghr10_title != "Malaria")

# Creating filters for older adults by sex, age, and malaria
older_male_adult_non_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age & cghr10_title != "Malaria")
older_female_adult_non_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age & cghr10_title != "Malaria")
older_adult_non_malaria <- adult %>% filter(death_age_group %in% older_adult_age & cghr10_title != "Malaria")

# Set mapping dataframe
mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10", "symp11", 
               "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count") 
)

# Testing out function with adult malaria
young_male_adult_nm_agg <- spatial_agg(gdf = dist,
                                    agg = young_male_adult_non_malaria,
                                    mapping = mapping,
                                    gdf_id = "distname", 
                                    agg_id = "district_cod",
                                    is_spatial_join = FALSE,
                                    count_col = "non_malaria_deaths")

young_female_adult_nm_agg <- spatial_agg(gdf = dist,
                                      agg = young_female_adult_non_malaria,
                                      mapping = mapping,
                                      gdf_id = "distname", 
                                      agg_id = "district_cod",
                                      is_spatial_join = FALSE,
                                      count_col = "non_malaria_deaths")

older_male_adult_nm_agg <- spatial_agg(gdf = dist,
                                    agg = older_male_adult_non_malaria,
                                    mapping = mapping,
                                    gdf_id = "distname", 
                                    agg_id = "district_cod",
                                    is_spatial_join = FALSE,
                                    count_col = "non_malaria_deaths")

older_female_adult_nm_agg <- spatial_agg(gdf = dist,
                                      agg = older_female_adult_non_malaria,
                                      mapping = mapping,
                                      gdf_id = "distname", 
                                      agg_id = "district_cod",
                                      is_spatial_join = FALSE,
                                      count_col = "non_malaria_deaths")

young_adult_nm_agg <- spatial_agg(gdf = dist,
                               agg = young_adult_non_malaria,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "non_malaria_deaths")

older_adult_nm_agg <- spatial_agg(gdf = dist,
                               agg = older_adult_non_malaria,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "non_malaria_deaths")

adult_agg <- spatial_agg(gdf = dist,
                         agg = adult,
                         mapping = mapping,
                         gdf_id = "distname", 
                         agg_id = "district_cod",
                         is_spatial_join = FALSE,
                         count_col = "all_deaths")

# Defining symptoms to be plotted
adult_symptoms <- c("fever", "abdominalProblem", "breathingProblem", "cough", "vomit",
                    "weightLoss")

# Running symptom_rate for each age group
yam_nm_symptom <- symptom_rate(age_sex_agg = young_male_adult_nm_agg,
                            all_agg = adult_agg, deaths = "non_malaria_deaths",
                            symptoms = adult_symptoms)
yaf_nm_symptom <- symptom_rate(age_sex_agg = young_female_adult_nm_agg,
                            all_agg = adult_agg, deaths = "non_malaria_deaths",
                            symptoms = adult_symptoms)
oam_nm_symptom <- symptom_rate(age_sex_agg = older_male_adult_nm_agg,
                            all_agg = adult_agg, deaths = "non_malaria_deaths",
                            symptoms = adult_symptoms)
oaf_nm_symptom <- symptom_rate(age_sex_agg = older_female_adult_nm_agg,
                            all_agg = adult_agg, deaths = "non_malaria_deaths",
                            symptoms = adult_symptoms)
young_adult_nm_symptom <- symptom_rate(age_sex_agg = young_adult_nm_agg,
                                    all_agg = adult_agg, deaths = "non_malaria_deaths",
                                    symptoms = adult_symptoms)
older_adult_nm_symptom <- symptom_rate(age_sex_agg = older_adult_nm_agg,
                                    all_agg = adult_agg, deaths = "non_malaria_deaths",
                                    symptoms = adult_symptoms)

# Creating plot series for each age group
yam_nm_plot <- create_plots(yam_nm_symptom, "Young Adult Male Non-Malaria Symptoms", "fig-yam-non_malaria-maps")
yaf_nm_plot <- create_plots(yaf_nm_symptom, "Young Adult Female Non-Malaria Symptoms", "fig-yaf-non_malaria-maps")
oam_nm_plot <- create_plots(oam_nm_symptom, "Older Adult Male Non-Malaria Symptoms", "fig-oam-non_malaria-maps")
oaf_nm_plot <- create_plots(oaf_nm_symptom, "Older Adult Female Non-Malaria Symptoms", "fig-oaf-non_malaria-maps")
young_adult_nm_plot <- create_plots(young_adult_nm_symptom, "Young Adult Non-Malaria Symptoms", "fig-ya-non-malaria-maps")
older_adult_nm_plot <- create_plots(older_adult_nm_symptom, "Older Adult Non-Malaria Symptoms", "fig-oa-non-malaria-maps")
