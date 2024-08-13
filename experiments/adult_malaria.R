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
library(rlang)

## Read data
# Reading in Adult Round 1 and Round 2 data
adult_r1 <- st_read("../tmp/data/healsl_rd1_adult_v1.csv")
adult_r2 <- st_read("../tmp/data/healsl_rd2_adult_v1.csv")

# Reading District Boundary file
dist <- st_read("../tmp/data/sl_dist_17_v2.geojson")

# Reading in GID r1 and r2 files 
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
adult <- adult %>% filter(death_age_group != "10-14")

adult <- adult %>%
    mutate(
        `WBD code` = str_trim(`WBD code`),
        type_of_cause = case_when(
            `WBD code` == "1H01" ~ "Malaria",
            str_starts(`WBD code`, "1") ~ "Infections",
            str_starts(`WBD code`, "2") | str_starts(`WBD code`, "3") ~ "Non-infections",
            TRUE ~ NA_character_)
        ) 

# Creating filters for different adult age/sex groups
young_adult <- adult %>% filter(death_age_group %in% young_adult_age)
young_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age)
young_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age)
older_adult <- adult %>% filter(death_age_group %in% older_adult_age)
older_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age)
older_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age)

# Creating filters for adults for malaria
adult_malaria <- adult %>% filter(type_of_cause == "Malaria")
young_adult_malaria <- adult %>% filter(death_age_group %in% young_adult_age & type_of_cause == "Malaria")
young_male_adult_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age & type_of_cause == "Malaria")
young_female_adult_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age & type_of_cause == "Malaria")
older_adult_malaria <- adult %>% filter(death_age_group %in% older_adult_age & type_of_cause == "Malaria")
older_male_adult_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age & type_of_cause == "Malaria")
older_female_adult_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age & type_of_cause == "Malaria")

# Creating filters for adults for infections
adult_infections <- adult %>% filter(type_of_cause == "Infections")
yam_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Male" & type_of_cause == "Infections")
yaf_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Female" & type_of_cause == "Infections")
oam_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Male" & type_of_cause == "Infections")
oaf_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Female" & type_of_cause == "Infections")
young_adult_infections <- adult %>% filter(death_age_group %in% young_adult_age & type_of_cause == "Infections")
older_adult_infections <- adult %>% filter(death_age_group %in% older_adult_age & type_of_cause == "Infections")

# Creating filters for adults for non-infections
adult_non_infections <- adult %>% filter(type_of_cause == "Non-infections")
yam_non_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Male" & type_of_cause == "Non-infections")
yaf_non_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Female" & type_of_cause == "Non-infections")
oam_non_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Male" & type_of_cause == "Non-infections")
oaf_non_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Female" & type_of_cause == "Non-infections")
young_adult_non_infections <- adult %>% filter(death_age_group %in% young_adult_age & type_of_cause == "Non-infections")
older_adult_non_infections <- adult %>% filter(death_age_group %in% older_adult_age & type_of_cause == "Non-infections")

# Making non-spatial tables----
non_spatial_adult <- non_spatial(age_group = adult, death_type = "type_of_cause", percentages = FALSE)
non_spatial_young_adult <- non_spatial(age_group = young_adult, death_type = "type_of_cause", percentages = FALSE)
non_spatial_yam <- non_spatial(age_group = young_male_adult, death_type = "type_of_cause", percentages = FALSE)
non_spatial_yaf <- non_spatial(age_group = young_female_adult, death_type = "type_of_cause", percentages = FALSE)
non_spatial_older_adult <- non_spatial(age_group = older_adult, death_type = "type_of_cause", percentages = FALSE)
non_spatial_oam <- non_spatial(age_group = older_male_adult, death_type = "type_of_cause", percentages = FALSE)
non_spatial_oaf <- non_spatial(age_group = older_female_adult, death_type = "type_of_cause", percentages = FALSE)

# Plotting heatmaps----
hm_adult <- hm(non_spatial_adult, "Adult (15-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-adult-heatmap", labels = TRUE, desc_order = FALSE)
hm_young_adult <- hm(non_spatial_young_adult, "Young Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-young-adult-heatmap", labels = TRUE, desc_order = FALSE)
hm_older_adult <- hm(non_spatial_older_adult, "Older Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-older-adult-heatmap", labels = TRUE, desc_order = FALSE)
hm_young_male_adult <- hm(non_spatial_yam, "Young Male Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-yam-heatmap", labels = TRUE, desc_order = FALSE)
hm_young_female_adult <- hm(non_spatial_yaf, "Young Female Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-yaf-heatmap", labels = TRUE, desc_order = FALSE)
hm_older_male_adult <- hm(non_spatial_oam, "Older Male Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-oam-heatmap", labels = TRUE, desc_order = FALSE)
hm_older_female_adult <- hm(non_spatial_oaf, "Older Female Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-oaf-heatmap", labels = TRUE, desc_order = FALSE)

# Set mapping dataframe
mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10", "symp11", 
               "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count") 
)

# Calculating aggregated symptom counts----
yam_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = young_male_adult_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

yaf_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = young_female_adult_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

oam_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = older_male_adult_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

oaf_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = older_female_adult_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

young_adult_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = young_adult_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

older_adult_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = older_adult_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

adult_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = adult_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

yam_infections_agg <- spatial_agg(
    gdf = dist,
    agg = yam_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

yaf_infections_agg <- spatial_agg(
    gdf = dist,
    agg = yaf_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

oam_infections_agg <- spatial_agg(
    gdf = dist,
    agg = oam_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

oaf_infections_agg <- spatial_agg(
    gdf = dist,
    agg = oaf_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

young_adult_infections_agg <- spatial_agg(
    gdf = dist,
    agg = young_adult_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

older_adult_infections_agg <- spatial_agg(
    gdf = dist,
    agg = older_adult_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

adult_infections_agg <- spatial_agg(
    gdf = dist,
    agg = adult_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

yam_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = yam_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

yaf_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = yaf_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

oam_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = oam_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

oaf_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = oaf_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

young_adult_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = young_adult_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

older_adult_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = older_adult_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

adult_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = adult_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

# Defining symptoms to be plotted----
adult_symptoms <- c("fever", "abdominalProblem", "breathingProblem", "cough", "vomit", "weightLoss", 
                    "chestPain", "unconscious", "paralysis", "looseStools", "urinaryProblem", "oedema", 
                    "skinProblems", "yellowEyes", "convulsions", "lumps")


# Running symptom_rate for each age group----
# Young Male Adults
yam_malaria_symptom <- symptom_rate(age_sex_agg = yam_malaria_agg,
                                    cod = "malaria",
                                    deaths = "deaths",
                                    symptoms = adult_symptoms)
yam_infections_symptom <- symptom_rate(age_sex_agg = yam_infections_agg,
                                       cod = "infections",
                                       deaths = "deaths",
                                       symptoms = adult_symptoms)
yam_non_infections_symptom <- symptom_rate(age_sex_agg = yam_non_infections_agg,
                                           cod = "non_infections",
                                           deaths = "deaths",
                                           symptoms = adult_symptoms)
yam_symptom <- bind_rows(yam_malaria_symptom, yam_infections_symptom, yam_non_infections_symptom)

# Young Female Adults
yaf_malaria_symptom <- symptom_rate(age_sex_agg = yaf_malaria_agg,
                                    cod = "malaria",
                                    deaths = "deaths",
                                    symptoms = adult_symptoms)
yaf_infections_symptom <- symptom_rate(age_sex_agg = yaf_infections_agg,
                                       cod = "infections",
                                       deaths = "deaths",
                                       symptoms = adult_symptoms)
yaf_non_infections_symptom <- symptom_rate(age_sex_agg = yaf_non_infections_agg,
                                           cod = "non_infections",
                                           deaths = "deaths",
                                           symptoms = adult_symptoms)
yaf_symptom <- bind_rows(yaf_malaria_symptom, yaf_infections_symptom, yaf_non_infections_symptom)

# Older Male Adults
oam_malaria_symptom <- symptom_rate(age_sex_agg = oam_malaria_agg,
                                    cod = "malaria",
                                    deaths = "deaths",
                                    symptoms = adult_symptoms)
oam_infections_symptom <- symptom_rate(age_sex_agg = oam_infections_agg,
                                       cod = "infections",
                                       deaths = "deaths",
                                       symptoms = adult_symptoms)
oam_non_infections_symptom <- symptom_rate(age_sex_agg = oam_non_infections_agg,
                                           cod = "non_infections",
                                           deaths = "deaths",
                                           symptoms = adult_symptoms)
oam_symptom <- bind_rows(oam_malaria_symptom, oam_infections_symptom, oam_non_infections_symptom)

# Older Female Adults
oaf_malaria_symptom <- symptom_rate(age_sex_agg = oaf_malaria_agg,
                                    cod = "malaria",
                                    deaths = "deaths",
                                    symptoms = adult_symptoms)
oaf_infections_symptom <- symptom_rate(age_sex_agg = oaf_infections_agg,
                                       cod = "infections",
                                       deaths = "deaths",
                                       symptoms = adult_symptoms)
oaf_non_infections_symptom <- symptom_rate(age_sex_agg = oaf_non_infections_agg,
                                           cod = "non_infections",
                                           deaths = "deaths",
                                           symptoms = adult_symptoms)
oaf_symptom <- bind_rows(oaf_malaria_symptom, oaf_infections_symptom, oaf_non_infections_symptom)

# Young Adults
young_adult_malaria_symptom <- symptom_rate(age_sex_agg = young_adult_malaria_agg,
                                            cod = "malaria",
                                            deaths = "deaths",
                                            symptoms = adult_symptoms)
young_adult_infections_symptom <- symptom_rate(age_sex_agg = young_adult_infections_agg,
                                               cod = "infections",
                                               deaths = "deaths",
                                               symptoms = adult_symptoms)
young_adult_non_infections_symptom <- symptom_rate(age_sex_agg = young_adult_non_infections_agg,
                                                   cod = "non_infections",
                                                   deaths = "deaths",
                                                   symptoms = adult_symptoms)
young_adult_symptom <- bind_rows(young_adult_malaria_symptom, young_adult_infections_symptom, young_adult_non_infections_symptom)

# Older Adults
older_adult_malaria_symptom <- symptom_rate(age_sex_agg = older_adult_malaria_agg,
                                            cod = "malaria",
                                            deaths = "deaths",
                                            symptoms = adult_symptoms)
older_adult_infections_symptom <- symptom_rate(age_sex_agg = older_adult_infections_agg,
                                               cod = "infections",
                                               deaths = "deaths",
                                               symptoms = adult_symptoms)
older_adult_non_infections_symptom <- symptom_rate(age_sex_agg = older_adult_non_infections_agg,
                                                   cod = "non_infections",
                                                   deaths = "deaths",
                                                   symptoms = adult_symptoms)
older_adult_symptom <- bind_rows(older_adult_malaria_symptom, older_adult_infections_symptom, older_adult_non_infections_symptom)

# Adults
adult_malaria_symptom <- symptom_rate(age_sex_agg = adult_malaria_agg,
                                            cod = "malaria",
                                            deaths = "deaths",
                                            symptoms = adult_symptoms)
adult_infections_symptom <- symptom_rate(age_sex_agg = adult_infections_agg,
                                               cod = "infections",
                                               deaths = "deaths",
                                               symptoms = adult_symptoms)
adult_non_infections_symptom <- symptom_rate(age_sex_agg = adult_non_infections_agg,
                                                   cod = "non_infections",
                                                   deaths = "deaths",
                                                   symptoms = adult_symptoms)
adult_symptom <- bind_rows(adult_malaria_symptom, adult_infections_symptom, adult_non_infections_symptom)

symptom_rate_tables <- c("yam_symptom", "yaf_symptom", "oam_symptom", "oaf_symptom", "young_adult_symptom", "older_adult_symptom", "adult_symptom")

for (srt in symptom_rate_tables) {
    
    df <- get(srt)
    
    df <- df %>%  mutate(symptoms = str_remove(symptoms, "_rate$")) %>% 
        mutate(denom_group = case_when( 
            str_ends(symptoms, "_malaria") ~ "Malaria", 
            str_ends(symptoms, "_non_infections") ~ "Non-Infections",
            str_ends(symptoms, "_infections") ~ "Infections"
        )) %>%
        mutate(symptoms = str_remove(symptoms, "_malaria$|_non_infections$|_infections$")) 
    
    df$rates[is.nan(df$rates)] <- 0
    
    assign(srt, df)
}

# Creating plot series for each age group----
yam_plot <- create_plots(yam_symptom, "Young Adult Male (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-yam-malaria-maps", label = TRUE, width = 42, height = 9)
yaf_plot <- create_plots(yaf_symptom, "Young Adult Female (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-yaf-malaria-maps", label = TRUE, width = 42, height = 9)
oam_plot <- create_plots(oam_symptom, "Older Adult Male (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-oam-malaria-maps", label = TRUE, width = 42, height = 9)
oaf_plot <- create_plots(oaf_symptom, "Older Adult Female (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-oaf-malaria-maps", label = TRUE, width = 42, height = 9)
young_adult_plot <- create_plots(young_adult_symptom, "Young Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-ya-malaria-maps", label = TRUE, width = 42, height = 9)
older_adult_plot <- create_plots(older_adult_symptom, "Older Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-oa-malaria-maps", label = TRUE, width = 42, height = 9)
adult_plot <- create_plots(adult_symptom, "Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-adult-malaria-maps", label = TRUE, width = 42, height = 9)
