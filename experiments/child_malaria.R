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
library(scales)

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
icd <- read_xlsx("../tmp/data/ICD-10 Version2016_Jun22_CM.xlsx")

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
    filter(is.na(p1_recon_icd) & is.na(p2_recon_icd) & is.na(adj_icd)) %>%
    mutate(final_icd = p1_icd)

# Assign wbd-10 title for corresponding record codes
child <- left_join(child, icd, by = "final_icd")

# Convert data type of District ID column
child$gid_dist <- as.integer(child$gid_dist)

child <- child %>%
    mutate(
        `WBD code` = str_trim(`WBD code`),
        type_of_cause = case_when(
            `WBD code` == "1H01" ~ "Malaria",
            str_starts(`WBD code`, "1") ~ "Infections",
            str_starts(`WBD code`, "2") |
                str_starts(`WBD code`, "3") ~ "Non-infections",
            TRUE ~ NA_character_
        )
    ) 

# Creating filters for young childs by sex, age, and malaria
male_child <- child %>% filter(sex_death == "Male")
female_child <- child %>% filter(sex_death == "Female")

child_malaria <- child %>% filter(type_of_cause == "Malaria")
male_child_malaria <- child %>% filter(sex_death == "Male" & type_of_cause == "Malaria")
female_child_malaria <- child %>% filter(sex_death == "Female" & type_of_cause == "Malaria")

child_infections <- child %>% filter(type_of_cause == "Infections")
male_child_infections <- child %>% filter(sex_death == "Male" & type_of_cause == "Infections")
female_child_infections <- child %>% filter(sex_death == "Female" & type_of_cause == "Infections")

child_non_infections <- child %>% filter(type_of_cause == "Non-infections")
male_child_non_infections <- child %>% filter(sex_death == "Male" & type_of_cause == "Non-infections")
female_child_non_infections <- child %>% filter(sex_death == "Female" & type_of_cause == "Non-infections")

# Creating non-spatial table of symptom and causes of death
non_spatial_children <- non_spatial(age_group = child, death_type = "type_of_cause", percentages = FALSE)
non_spatial_cm <- non_spatial(age_group = male_child, death_type = "type_of_cause", percentages = FALSE)
non_spatial_cf <- non_spatial(age_group = female_child, death_type = "type_of_cause", percentages = FALSE)

# Creating heat map with non-spatial table
hm_children <- hm(non_spatial_children, "Child (1m-11y) Deaths by Symptom\nSierra Leone 2019-2022", "fig-child-heatmap", labels = TRUE, desc_order = TRUE)
hm_male_child <- hm(non_spatial_cm, "Male Child (1m-11y) Deaths by Symptom\nSierra Leone 2019-2022", "fig-cm-heatmap", labels = TRUE, desc_order = TRUE)
hm_female_child <- hm(non_spatial_cf, "Female Child (1m-11y) Deaths by Symptom\nSierra Leone 2019-2022", "fig-cf-heatmap", labels = TRUE, desc_order = TRUE)

# Set mapping dataframe
mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10",
               "symp11", "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count", "count", "count") 
)

# childing out function with child malaria
cm_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = male_child_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

cf_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = female_child_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

child_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = child_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

cm_infections_agg <- spatial_agg(
    gdf = dist,
    agg = male_child_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

cf_infections_agg <- spatial_agg(
    gdf = dist,
    agg = female_child_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

child_infections_agg <- spatial_agg(
    gdf = dist,
    agg = child_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

cm_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = male_child_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

cf_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = female_child_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

child_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = child_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

# Defining symptoms to be plotted
child_symptoms <- c("fever", "weightLoss", "difficultyBreathing", "vomit", "headache", "cough",
                    "convulsion", "yellowEyes", "looseStools", "unconscious", "abdominalProblem",
                    "neckStiffness", "oedema", "skinRash", "ulcers", "chestPain", "difficultySwallowing",
                    "urinaryProblem", "bleeding", "lumps", "paralysis")


# Running symptom_rate for each sex group
# Children
child_malaria_symptom <- symptom_rate(age_sex_agg = child_malaria_agg,
                                    cod = "malaria",
                                    deaths = "deaths",
                                    symptoms = child_symptoms)
child_infections_symptom <- symptom_rate(age_sex_agg = child_infections_agg,
                                       cod = "infections",
                                       deaths = "deaths",
                                       symptoms = child_symptoms)
child_non_infections_symptom <- symptom_rate(age_sex_agg = child_non_infections_agg,
                                           cod = "non_infections",
                                           deaths = "deaths",
                                           symptoms = child_symptoms)
child_symptom <- bind_rows(child_malaria_symptom, child_infections_symptom, child_non_infections_symptom)
child_symptom <- child_symptom %>% mutate(
    age_range = "<1-11",
    age_group = "Child",
    sex = "Both")

# Male Children
cm_malaria_symptom <- symptom_rate(age_sex_agg = cm_malaria_agg,
                                    cod = "malaria",
                                    deaths = "deaths",
                                    symptoms = child_symptoms)
cm_infections_symptom <- symptom_rate(age_sex_agg = cm_infections_agg,
                                       cod = "infections",
                                       deaths = "deaths",
                                       symptoms = child_symptoms)
cm_non_infections_symptom <- symptom_rate(age_sex_agg = cm_non_infections_agg,
                                           cod = "non_infections",
                                           deaths = "deaths",
                                           symptoms = child_symptoms)
cm_symptom <- bind_rows(cm_malaria_symptom, cm_infections_symptom, cm_non_infections_symptom)
cm_symptom <- cm_symptom %>% mutate(
    age_range = "<1-11",
    age_group = "Child",
    sex = "Male")

# Female Children
cf_malaria_symptom <- symptom_rate(age_sex_agg = cf_malaria_agg,
                                    cod = "malaria",
                                    deaths = "deaths",
                                    symptoms = child_symptoms)
cf_infections_symptom <- symptom_rate(age_sex_agg = cf_infections_agg,
                                       cod = "infections",
                                       deaths = "deaths",
                                       symptoms = child_symptoms)
cf_non_infections_symptom <- symptom_rate(age_sex_agg = cf_non_infections_agg,
                                           cod = "non_infections",
                                           deaths = "deaths",
                                           symptoms = child_symptoms)
cf_symptom <- bind_rows(cf_malaria_symptom, cf_infections_symptom, cf_non_infections_symptom)
cf_symptom <- cf_symptom %>% mutate(
    age_range = "<1-11",
    age_group = "Child",
    sex = "Female")

symptom_rate_tables <- c("child_symptom", "cm_symptom", "cf_symptom")

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

all_child_symptom <- bind_rows(child_symptom, cm_symptom, cf_symptom)
 
# Creating maps for each age group
cm_plot <-
    create_plots(
        all_child_symptom,
        "Child Male (1m-11y) Deaths by Symptom\nSierra Leone 2019-2022\n(n = 1095)",
        "fig-cm-malaria-maps",
        label = TRUE,
        width = 10,
        height = 47,
        age_range = "<1-11",
        age_group = "Child",
        sex = "Male",
        orientation = "portrait"
    )

cf_plot <-
    create_plots(
        all_child_symptom,
        "Child Female (1m-11y) Deaths by Symptom\nSierra Leone 2019-2022\n(n = 1085)",
        "fig-cf-malaria-maps",
        label = TRUE,
        width = 10,
        height = 47,
        age_range = "<1-11",
        age_group = "Child",
        sex = "Female",
        orientation = "portrait"
    )

child_plot <-
    create_plots(
        all_child_symptom,
        "Child (1m-11y) Deaths by Symptom\nSierra Leone 2019-2022\n(n = 2180)",
        "fig-child-malaria-maps",
        label = TRUE,
        width = 10,
        height = 47,
        age_range = "<1-11",
        age_group = "Child",
        sex = "Both",
        orientation = "portrait"
    )

