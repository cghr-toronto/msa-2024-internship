source("../src/spatial_agg.R")
source("../experiments/functions.R")

# Loading packages for being able to manipulate and plot spatial data
library(sf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(prettymapr)
library(patchwork)
library(readxl)
library(glue)
library(forcats)
library(spdep)

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
icd <- read_xlsx("../tmp/data/ICD-10 Version2016_Jun22_CM.xlsx")

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
    filter(is.na(p1_recon_icd_cod) & is.na(p2_recon_icd_cod) & is.na(adj_icd_cod)) %>%
    mutate(final_icd_cod = p1_icd_cod)


# Assign WBD-10 title for corresponding record codes
adult <- left_join(adult, icd, by = setNames("final_icd", "final_icd_cod"))

# Creating age ranges for adults
young_adult_age <- c("15-19", "20-24", "25-29", "30-34", "35-39")
older_adult_age <- c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

# List of causes of death
infections <- c("Fever of unknown origin", 
                "Meningitis/encephalitis", 
                "Selected vaccine-preventable", 
                "Tuberculosis", 
                "Diarrhoea",
                "Hepatitis", 
                "Sexually-transmitted infections",
                "Respiratory infections",
                "Post COVID-19 condition",
                "Multisystem inflammatory syndrme associated with COVID-19",
                "Need for immunization against COVID-19",
                "Other infectious and parasitic")
    
infections_2 <- c("Other chronic respiratory infections",
                  "Covid")

# Trim whitespaces in adult columns
adult$COD <- str_trim(adult$COD)
adult$`ICD-Chapter`<- str_trim(adult$`ICD-Chapter`)

adult <- adult %>% mutate(type_of_death = case_when(
    `WBD category` == "Malaria" ~ "Malaria",
    (`WBD category` %in% infections) | 
        (`COD Group (Cathy)` %in% infections_2) | 
        (`COD` == "Chronic viral hepatitis") ~ "Infections",
    TRUE ~ "Non-infections")) %>%
    mutate(type_of_death = if_else(is.na(`WBD category`), NA_character_, type_of_death))

# Creating filters for different adult age/sex groups
young_adult <- adult %>% filter(death_age_group %in% young_adult_age)
young_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age)
young_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age)
older_adult <- adult %>% filter(death_age_group %in% older_adult_age)
older_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age)
older_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age)

# Creating filters for adults for malaria
adult_malaria <- adult %>% filter(type_of_death == "Malaria")
young_adult_malaria <- adult %>% filter(death_age_group %in% young_adult_age & type_of_death == "Malaria")
young_male_adult_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age & type_of_death == "Malaria")
young_female_adult_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age & type_of_death == "Malaria")
older_adult_malaria <- adult %>% filter(death_age_group %in% older_adult_age & type_of_death == "Malaria")
older_male_adult_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age & type_of_death == "Malaria")
older_female_adult_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age & type_of_death == "Malaria")

# Creating filters for adults for infections
adult_infections <- adult %>% filter(type_of_death == "Infections")
yam_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Male" & type_of_death == "Infections")
yaf_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Female" & type_of_death == "Infections")
oam_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Male" & type_of_death == "Infections")
oaf_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Female" & type_of_death == "Infections")
young_adult_infections <- adult %>% filter(death_age_group %in% young_adult_age & type_of_death == "Infections")
older_adult_infections <- adult %>% filter(death_age_group %in% older_adult_age & type_of_death == "Infections")

# Creating filters for adults for non-infections
adult_non_infections <- adult %>% filter(type_of_death == "Non-infections")
yam_non_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Male" & type_of_death == "Non-infections")
yaf_non_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Female" & type_of_death == "Non-infections")
oam_non_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Male" & type_of_death == "Non-infections")
oaf_non_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Female" & type_of_death == "Non-infections")
young_adult_non_infections <- adult %>% filter(death_age_group %in% young_adult_age & type_of_death == "Non-infections")
older_adult_non_infections <- adult %>% filter(death_age_group %in% older_adult_age & type_of_death == "Non-infections")

# Set mapping dataframe
mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10", "symp11", 
               "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count") 
)

# Calculating aggregated symptom counts----
yam_malaria_agg <- spatial_agg(gdf = dist,
                                 agg = young_male_adult_malaria,
                                 mapping = mapping,
                                 gdf_id = "distname", 
                                 agg_id = "district_cod",
                                 is_spatial_join = FALSE,
                                 count_col = "deaths")

yaf_malaria_agg <- spatial_agg(gdf = dist,
                                        agg = young_female_adult_malaria,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "deaths")

oam_malaria_agg <- spatial_agg(gdf = dist,
                                        agg = older_male_adult_malaria,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "deaths")

oaf_malaria_agg <- spatial_agg(gdf = dist,
                                        agg = older_female_adult_malaria,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "deaths")

young_adult_malaria_agg <- spatial_agg(gdf = dist,
                                      agg = young_adult_malaria,
                                      mapping = mapping,
                                      gdf_id = "distname", 
                                      agg_id = "district_cod",
                                      is_spatial_join = FALSE,
                                      count_col = "deaths")

older_adult_malaria_agg <- spatial_agg(gdf = dist,
                                      agg = older_adult_malaria,
                                      mapping = mapping,
                                      gdf_id = "distname", 
                                      agg_id = "district_cod",
                                      is_spatial_join = FALSE,
                                      count_col = "deaths")

yam_infections_agg <- spatial_agg(gdf = dist,
                               agg = yam_infections,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "deaths")

yaf_infections_agg <- spatial_agg(gdf = dist,
                               agg = yaf_infections,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "deaths")

oam_infections_agg <- spatial_agg(gdf = dist,
                               agg = oam_infections,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "deaths")

oaf_infections_agg <- spatial_agg(gdf = dist,
                               agg = oaf_infections,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "deaths")

young_adult_infections_agg <- spatial_agg(gdf = dist,
                                       agg = young_adult_infections,
                                       mapping = mapping,
                                       gdf_id = "distname", 
                                       agg_id = "district_cod",
                                       is_spatial_join = FALSE,
                                       count_col = "deaths")

older_adult_infections_agg <- spatial_agg(gdf = dist,
                                       agg = older_adult_infections,
                                       mapping = mapping,
                                       gdf_id = "distname", 
                                       agg_id = "district_cod",
                                       is_spatial_join = FALSE,
                                       count_col = "deaths")

yam_non_infections_agg <- spatial_agg(gdf = dist,
                               agg = yam_non_infections,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "deaths")

yaf_non_infections_agg <- spatial_agg(gdf = dist,
                               agg = yaf_non_infections,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "deaths")

oam_non_infections_agg <- spatial_agg(gdf = dist,
                               agg = oam_non_infections,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "deaths")

oaf_non_infections_agg <- spatial_agg(gdf = dist,
                               agg = oaf_non_infections,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "deaths")

young_adult_non_infections_agg <- spatial_agg(gdf = dist,
                                       agg = young_adult_non_infections,
                                       mapping = mapping,
                                       gdf_id = "distname", 
                                       agg_id = "district_cod",
                                       is_spatial_join = FALSE,
                                       count_col = "deaths")

older_adult_non_infections_agg <- spatial_agg(gdf = dist,
                                       agg = older_adult_non_infections,
                                       mapping = mapping,
                                       gdf_id = "distname", 
                                       agg_id = "district_cod",
                                       is_spatial_join = FALSE,
                                       count_col = "deaths")

# Making non-spatial tables----
non_spatial_adult <- non_spatial(adult)
non_spatial_young_adult <- non_spatial(young_adult)
non_spatial_yam <- non_spatial(young_male_adult)
non_spatial_yaf <- non_spatial(young_female_adult)
non_spatial_older_adult <- non_spatial(older_adult)
non_spatial_oam <- non_spatial(older_male_adult)
non_spatial_oaf <- non_spatial(older_female_adult)

# Plotting heatmaps----
hm_adult <- hm(non_spatial_adult, glue("Adult (15-69 Years) Deaths by Symptom\nSierra Leone 2019-2022"), "fig-adult-heatmap", labels = FALSE)
hm_young_adult <- hm(non_spatial_young_adult, "Young Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-young-adult-heatmap", labels = FALSE)
hm_older_adult <- hm(non_spatial_older_adult, "Older Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-older-adult-heatmap", labels = FALSE)
hm_young_male_adult <- hm(non_spatial_yam, "Young Male Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-yam-heatmap", labels = FALSE)
hm_young_female_adult <- hm(non_spatial_yaf, "Young Female Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-yaf-heatmap", labels = FALSE)
hm_older_male_adult <- hm(non_spatial_oam, "Older Male Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-oam-heatmap", labels = FALSE)
hm_older_female_adult <- hm(non_spatial_oaf, "Older Female Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-oaf-heatmap", labels = FALSE)

# Defining symptoms to be plotted----
adult_symptoms <- c("fever", "abdominalProblem", "breathingProblem", "cough", "vomit", "weightLoss")

# Running symptom_rate for each age group----
yam_symptom <- symptom_rate(age_sex_malaria_agg = yam_malaria_agg,
                            age_sex_infections_agg = yam_infections_agg,
                            age_sex_non_infections_agg = yam_non_infections_agg,
                            deaths = "deaths",
                            symptoms = adult_symptoms)

yaf_symptom <- symptom_rate(age_sex_malaria_agg = yaf_malaria_agg,
                            age_sex_infections_agg = yaf_infections_agg,
                            age_sex_non_infections_agg = yaf_non_infections_agg,
                            deaths = "deaths",
                            symptoms = adult_symptoms)

oam_symptom <- symptom_rate(age_sex_malaria_agg = oam_malaria_agg,
                            age_sex_infections_agg = oam_infections_agg,
                            age_sex_non_infections_agg = oam_non_infections_agg,
                            deaths = "deaths",
                            symptoms = adult_symptoms)

oaf_symptom <- symptom_rate(age_sex_malaria_agg = oaf_malaria_agg,
                            age_sex_infections_agg = oaf_infections_agg,
                            age_sex_non_infections_agg = oaf_non_infections_agg,
                            deaths = "deaths",
                            symptoms = adult_symptoms)

young_adult_symptom <- symptom_rate(age_sex_malaria_agg = young_adult_malaria_agg,
                                    age_sex_infections_agg = young_adult_infections_agg,
                                    age_sex_non_infections_agg = young_adult_non_infections_agg,
                                    deaths = "deaths",
                                    symptoms = adult_symptoms)

older_adult_symptom <- symptom_rate(age_sex_malaria_agg = older_adult_malaria_agg,
                                    age_sex_infections_agg = older_adult_infections_agg,
                                    age_sex_non_infections_agg = older_adult_non_infections_agg,
                                    deaths = "deaths",
                                    symptoms = adult_symptoms)

# Creating plot series for each age group----
yam_plot <- create_plots(yam_symptom, "Young Adult Male (15-39 Years) Malaria Symptoms", "fig-yam-malaria-maps", label = FALSE)
yaf_plot <- create_plots(yaf_symptom, "Young Adult Female (15-39 Years) Malaria Symptoms", "fig-yaf-malaria-maps", label = FALSE)
oam_plot <- create_plots(oam_symptom, "Older Adult Male (40-69 Years) Malaria Symptoms", "fig-oam-malaria-maps", label = FALSE)
oaf_plot <- create_plots(oaf_symptom, "Older Adult Female (40-69 Years) Malaria Symptoms", "fig-oaf-malaria-maps", label = FALSE)
young_adult_plot <- create_plots(young_adult_symptom, "Young Adult (15-39 Years) Malaria Symptoms", "fig-ya-malaria-maps", label = FALSE)
older_adult_plot <- create_plots(older_adult_symptom, "Older Adult (40-69 Years) Malaria Symptoms", "fig-oa-malaria-maps", label = FALSE)

