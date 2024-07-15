source("../src/spatial_agg.R")

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
    mutate(final_icd_cod = case_when(!is.na(adj_icd_cod) ~ adj_icd_cod,  # Use adj_icd if it is not NA
                                     is.na(adj_icd_cod) & !is.na(p1_recon_icd_cod) & !is.na(p2_recon_icd_cod) ~ p1_recon_icd_cod,  # Use p1_recon_icd if adj_icd is NA and both p1_recon_icd and p2_recon_icd are not NA
                                     is.na(adj_icd_cod) & is.na(p1_recon_icd_cod) & is.na(p2_recon_icd_cod) ~ p1_icd_cod,  # Use p1_icd if both adj_icd and recon_icd are NA
                                     TRUE ~ NA_character_  # Default case, if none of the above conditions are met
    )
    ) 

# Assign CGHR-10 title for corresponding record codes
adult <- left_join(adult, icd, by = setNames("final_icd", "final_icd_cod"))

# Creating age ranges for adults
young_adult_age <- c("15-19", "20-24", "25-29", "30-34", "35-39")
older_adult_age <- c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

# List of causes of death
infections <- c("Acute respiratory infections", 
                "Digestive diseases", 
                "Fever of unknown origin", 
                "Meningitis/encephalitis", 
                "Other chronic respiratory infections",
                "Other infectious diseases", 
                "HIV/AIDS", 
                "Hepatitis", 
                "Severe Localized Infection", 
                "Selected vaccine preventable diseases", 
                "Other sexually transmitted infections (excl. HIV/AIDS)",
                "Tuberculosis", 
                "Diarrhoea",
                "Severe Systemic Infection",
                "Covid",
                "Measles",
                "Hepatitis",
                "Helminthiases",
                "Arthropod-borne viral fevers",
                "Rabies",
                "Syphilis",
                "Tetanus",
                "History of Covid-19")

# Trim whitespaces in COD column
adult$COD <- str_trim(adult$COD)

# Creating filters for different adult age/sex groups
young_adult <- adult %>% filter(death_age_group %in% young_adult_age)
young_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age)
young_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age)
older_adult <- adult %>% filter(death_age_group %in% older_adult_age)
older_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age)
older_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age)

# Creating filters for adults for malaria
adult_malaria <- adult %>% filter(`COD Group (Cathy)` == "Malaria")
young_adult_malaria <- adult %>% filter(death_age_group %in% young_adult_age & `COD Group (Cathy)` == "Malaria")
young_male_adult_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age & `COD Group (Cathy)` == "Malaria")
young_female_adult_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age & `COD Group (Cathy)` == "Malaria")
older_adult_malaria <- adult %>% filter(death_age_group %in% older_adult_age & `COD Group (Cathy)` == "Malaria")
older_male_adult_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age & `COD Group (Cathy)` == "Malaria")
older_female_adult_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age & `COD Group (Cathy)` == "Malaria")

# Creating filters for adults for infections
adult_infections <- adult %>% filter((`COD Group (Cathy)` %in% infections) | (`COD` == "Chronic viral hepatitis"))
yam_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Male" & (`COD Group (Cathy)` %in% infections) | (`COD` == "Chronic viral hepatitis"))
yaf_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Female" & (`COD Group (Cathy)` %in% infections) | (`COD` == "Chronic viral hepatitis"))
oam_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Male" & (`COD Group (Cathy)` %in% infections) | (`COD` == "Chronic viral hepatitis"))
oaf_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Female" & (`COD Group (Cathy)` %in% infections) | (`COD` == "Chronic viral hepatitis"))
young_adult_infections <- adult %>% filter(death_age_group %in% young_adult_age & (`COD Group (Cathy)` %in% infections) | (`COD` == "Chronic viral hepatitis"))
older_adult_infections <- adult %>% filter(death_age_group %in% older_adult_age & (`COD Group (Cathy)` %in% infections) | (`COD` == "Chronic viral hepatitis"))

# Creating filters for adults for non-infections
adult_non_infections <- adult %>% filter((!`COD Group (Cathy)` %in% infections) & `COD Group (Cathy)` != "Malaria" & `COD` != "Chronic viral hepatitis")
yam_non_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Male" & (!`COD Group (Cathy)` %in% infections) & `COD Group (Cathy)` != "Malaria" & `COD` != "Chronic viral hepatitis")
yaf_non_infections <- adult %>% filter(death_age_group %in% young_adult_age & sex_death == "Female" & (!`COD Group (Cathy)` %in% infections) & `COD Group (Cathy)` != "Malaria" & `COD` != "Chronic viral hepatitis")
oam_non_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Male" & (!`COD Group (Cathy)` %in% infections) & `COD Group (Cathy)` != "Malaria" & `COD` != "Chronic viral hepatitis")
oaf_non_infections <- adult %>% filter(death_age_group %in% older_adult_age & sex_death == "Female" & (!`COD Group (Cathy)` %in% infections) & `COD Group (Cathy)` != "Malaria" & `COD` != "Chronic viral hepatitis")
young_adult_non_infections <- adult %>% filter(death_age_group %in% young_adult_age & (!`COD Group (Cathy)` %in% infections) & `COD Group (Cathy)` != "Malaria" & `COD` != "Chronic viral hepatitis")
older_adult_non_infections <- adult %>% filter(death_age_group %in% older_adult_age & (!`COD Group (Cathy)` %in% infections) & `COD Group (Cathy)` != "Malaria" & `COD` != "Chronic viral hepatitis")

# Set mapping dataframe
mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10", "symp11", 
               "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count") 
)

# Testing out function with adult malaria
yam_malaria_agg <- spatial_agg(gdf = dist,
                               agg = young_male_adult_malaria,
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

yam_non_infections_agg <- spatial_agg(gdf = dist,
                                      agg = yam_non_infections,
                                      mapping = mapping,
                                      gdf_id = "distname", 
                                      agg_id = "district_cod",
                                      is_spatial_join = FALSE,
                                      count_col = "deaths")


cod_rate <- function(
        age_sex_agg,
        cod,
        symptoms,
        deaths){
    
    # Remove geometry from aggregated dataframe
    age_sex_without_geometry <- age_sex_agg  %>%
        as_tibble() %>%
        select(-geometry, -deaths, -distname)
    
    # Creating spatial symptom count
    result <- age_sex_without_geometry %>%
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
        left_join(age_sex_agg %>% select(gid, geometry, deaths, distname), by = "gid")
    
    # Create rate columns for malaria symptoms
    for (symptom in symptoms) {
        rate_column <- paste0(symptom, "_", cod, "_rate")
        spatial[[rate_column]] <- (spatial[[symptom]] / spatial[[deaths]]) * 100
        spatial[[rate_column]] <- round(spatial[[rate_column]], 2)
    }
    
    # Print the wide format
    cat("\nWide format:\n")
    print(spatial)
    
    # Convert spatial to an sf and reproject crs
    spatial <- spatial %>% st_as_sf(sf_column_name = "geometry") %>% st_transform(32628)
    
    out <- spatial %>% 
        pivot_longer(cols = ends_with("rate"),
                     names_to = "symptoms", 
                     values_to = "rates") %>%
        select(gid, symptoms, rates)
    
    return(out)
}

# Function for creating rates for aggregated results
symptom_rate <- function(
        age_sex_malaria_agg,
        age_sex_infections_agg,
        age_sex_non_infections_agg,
        deaths,
        symptoms){
    
    malaria_rates <- cod_rate(age_sex_agg = age_sex_malaria_agg,
                              cod = "malaria",
                              symptoms = symptoms,
                              deaths = deaths)
    
    infection_rates <- cod_rate(age_sex_agg = age_sex_infections_agg,
                                cod = "infections",
                                symptoms = symptoms,
                                deaths = deaths)
    
    non_infection_rates <- cod_rate(age_sex_agg = age_sex_non_infections_agg,
                                    cod = "non_infections",
                                    symptoms = symptoms,
                                    deaths = deaths)
    
    comb_rows <- bind_rows(malaria_rates, infection_rates, non_infection_rates)
    
    # Pivoted spatial table to show rates for each symptom
    out <- comb_rows %>%  mutate(symptoms = str_remove(symptoms, "_rate$")) %>% 
        mutate(denom_group = case_when( 
            str_ends(symptoms, "_malaria") ~ "Malaria", 
            str_ends(symptoms, "_non_infections") ~ "Non-Infections",
            str_ends(symptoms, "_infections") ~ "Infections"
        )) %>%
        mutate(symptoms = str_remove(symptoms, "_malaria$|_non_infections$|_infections$"))
    
    return(out)
    
}

# Defining symptoms to be plotted
adult_symptoms <- c("fever", "abdominalProblem", "breathingProblem", "cough", "vomit",
                    "weightLoss")

# Running symptom_rate for each age group
yam_symptom <- symptom_rate(age_sex_malaria_agg = yam_malaria_agg,
                            age_sex_infections_agg = yam_infections_agg,
                            age_sex_non_infections_agg = yam_non_infections_agg,
                            deaths = "deaths",
                            symptoms = adult_symptoms)


yam_test <- yam_symptom %>% filter(symptoms == "cough")

map <- ggplot(data = yam_test) +
    geom_sf(aes(fill=(rates))) +
    guides(fill = guide_legend()) +
    scale_fill_continuous(breaks = scales::pretty_breaks(n = 6)) +
    ggtitle("Testing cough") +
    geom_sf_label(aes(label = rates), size = 1.8) +
    theme_minimal() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 20)) 

map