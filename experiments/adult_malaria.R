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

# Trip whitespaces in COD column
adult$COD <- str_trim(adult$COD)

# Creating filters for young adults by sex, age, and malaria
young_adult_malaria <- adult %>% filter(death_age_group %in% young_adult_age & `COD Group (Cathy)` == "Malaria")
young_male_adult_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age & `COD Group (Cathy)` == "Malaria")
young_female_adult_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age & `COD Group (Cathy)` == "Malaria")
young_adult <- adult %>% filter(death_age_group %in% young_adult_age)
young_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age)
young_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age)

# Creating filters for older adults by sex, age, and malaria
older_adult_malaria <- adult %>% filter(death_age_group %in% older_adult_age & `COD Group (Cathy)` == "Malaria")
older_male_adult_malaria <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age & `COD Group (Cathy)` == "Malaria")
older_female_adult_malaria <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age & `COD Group (Cathy)` == "Malaria")
older_adult <- adult %>% filter(death_age_group %in% older_adult_age)
older_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age)
older_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age)

adult_malaria <- adult %>% filter(`COD Group (Cathy)` == "Malaria")
adult_infections <- adult %>% filter((`COD Group (Cathy)` %in% infections) | (`COD` == "Chronic viral hepatitis"))
adult_non_infections <- adult %>% filter((!`COD Group (Cathy)` %in% infections) & `COD Group (Cathy)` != "Malaria" & `COD` != "Chronic viral hepatitis")

# Set mapping dataframe
mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10", "symp11", 
               "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count") 
)

# Testing out function with adult malaria
young_male_adult_agg <- spatial_agg(gdf = dist,
                                 agg = young_male_adult_malaria,
                                 mapping = mapping,
                                 gdf_id = "distname", 
                                 agg_id = "district_cod",
                                 is_spatial_join = FALSE,
                                 count_col = "deaths")

young_female_adult_agg <- spatial_agg(gdf = dist,
                                        agg = young_female_adult_malaria,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "deaths")

older_male_adult_agg <- spatial_agg(gdf = dist,
                                        agg = older_male_adult_malaria,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "deaths")

older_female_adult_agg <- spatial_agg(gdf = dist,
                                        agg = older_female_adult_malaria,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "deaths")

young_adult_agg <- spatial_agg(gdf = dist,
                                      agg = young_adult_malaria,
                                      mapping = mapping,
                                      gdf_id = "distname", 
                                      agg_id = "district_cod",
                                      is_spatial_join = FALSE,
                                      count_col = "deaths")

older_adult_agg <- spatial_agg(gdf = dist,
                                      agg = older_adult_malaria,
                                      mapping = mapping,
                                      gdf_id = "distname", 
                                      agg_id = "district_cod",
                                      is_spatial_join = FALSE,
                                      count_col = "deaths")

adult_malaria_agg <- spatial_agg(gdf = dist,
                               agg = adult_malaria,
                               mapping = mapping,
                               gdf_id = "distname", 
                               agg_id = "district_cod",
                               is_spatial_join = FALSE,
                               count_col = "malaria_deaths")

adult_infection_agg <- spatial_agg(gdf = dist,
                                    agg = adult_infections,
                                    mapping = mapping,
                                    gdf_id = "distname", 
                                    agg_id = "district_cod",
                                    is_spatial_join = FALSE,
                                    count_col = "infection_deaths")

adult_non_infection_agg <- spatial_agg(gdf = dist,
                                    agg = adult_non_infections,
                                    mapping = mapping,
                                    gdf_id = "distname", 
                                    agg_id = "district_cod",
                                    is_spatial_join = FALSE,
                                    count_col = "non_infection_deaths")

adult_agg <- spatial_agg(gdf = dist,
                         agg = adult,
                         mapping = mapping,
                         gdf_id = "distname", 
                         agg_id = "district_cod",
                         is_spatial_join = FALSE,
                         count_col = "all_deaths")

# Creating PDF export parameters
pdf_print <- function(series, title){
    
    pdf_output_dir <- "../figures/"
    
    jpeg_output_dir <- "../figures.jpgs/"
    
    pdf_title <- paste0(pdf_output_dir, title, ".pdf")
    
    jpeg_title <- paste0(jpeg_output_dir, title, ".jpeg")
    
    ggsave(pdf_title, plot = series, device = "pdf", width = 26, height = 13)
    
    ggsave(jpeg_title, plot = series, device = "jpeg", width = 26, height = 13)
    
}

# Creating non-spatial table of symptom and causes of death
non_spatial <- function(age_group){
    
    ns <- pivot_longer(age_group, cols = starts_with("symp"), # Matches columns starting with "symp" followed by dig
                       names_to = "symptom", # New column to store the symptom names
                       values_to = "value" # New column to store the counts
    ) %>% group_by(`COD Group (Cathy)`, value) %>%
        summarise(count = n(), .groups = 'drop') %>%
        arrange(`COD Group (Cathy)`, value) %>%
        pivot_wider(
            names_from = value,   # The values in the 'value' column will become column names
            values_from = count,  # The values in the 'count' column will fill the new columns
            values_fill = list(count = 0)  # Fill missing values with 0
        )
    
    # Creating count for deaths per cause in non-spatial
    death_count <- age_group %>% count(`COD Group (Cathy)`, sort = TRUE, name = "deaths")
    ns <- ns %>% left_join(death_count, by = "COD Group (Cathy)")
    colnames(ns)[colnames(ns) == "COD Group (Cathy)"] <- "cause_of_death"
    
    ns <- ns %>%
        mutate(is_malaria = ifelse(cause_of_death == "Malaria", 1, 0)) %>%
        arrange(is_malaria) %>%
        select(-is_malaria) %>%
        mutate(type_of_cause = case_when(
            cause_of_death == "Malaria" ~ "Malaria",
            cause_of_death %in% infections ~ "Infections",
            TRUE ~ "Non-infections"
        ))
    
    return(ns)
}

non_spatial_adult <- non_spatial(adult)
non_spatial_young_adult <- non_spatial(young_adult)
non_spatial_yam <- non_spatial(young_male_adult)
non_spatial_yaf <- non_spatial(young_female_adult)
non_spatial_older_adult <- non_spatial(older_adult)
non_spatial_oam <- non_spatial(older_male_adult)
non_spatial_oaf <- non_spatial(older_female_adult)


# Creating heat map with non-spatial table
hm <- function(ns_table, hm_title, pdf_title) {
    
    heat <- pivot_longer(ns_table, cols = -c(cause_of_death, type_of_cause),
                         names_to = "symptoms",
                         values_to = "counts") %>%
        filter(cause_of_death != "NA" & symptoms != "NA" & symptoms != "deaths") %>% 
        group_by(type_of_cause, symptoms) %>%
        summarise(total_count = sum(counts))
    
    heat$type_of_cause <- factor(heat$type_of_cause, levels = c("Non-infections", "Infections", "Malaria"))
    
    heat_map_plot <- ggplot(heat, aes(symptoms, type_of_cause)) +
        geom_tile(aes(fill = total_count)) +
        geom_text(aes(label = round(total_count, 1))) +
        scale_fill_gradient(low = "white", high = "red") +
        scale_x_discrete(position = "top") +
        theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 0, vjust = 0, margin = margin(t = 30, r = 30)),
              axis.title.x = element_blank(),
              panel.grid.major = element_blank(),  # Remove major grid lines
              panel.grid.minor = element_blank(),  # Remove minor grid lines
              panel.background = element_blank()) +
        ggtitle(hm_title)
    
    # Exporting heat map as pdf
    out <- pdf_print(heat_map_plot, pdf_title)
    
    return(out)
    
}

hm_adult <- hm(non_spatial_adult, "Adult Symptom Heatmap", "fig-adult-heatmap")
hm_young_adult <- hm(non_spatial_young_adult, "Young Adult Symptom Heatmap", "fig-young-adult-heatmap")
hm_older_adult <- hm(non_spatial_older_adult, "Older Adult Symptom Heatmap", "fig-older-adult-heatmap")
hm_young_male_adult <- hm(non_spatial_yam, "Young Male Adult Symptom Heatmap", "fig-yam-heatmap")
hm_young_female_adult <- hm(non_spatial_yaf, "Young Female Adult Symptom Heatmap", "fig-yaf-heatmap")
hm_older_male_adult <- hm(non_spatial_oam, "Older Male Adult Symptom Heatmap", "fig-oam-heatmap")
hm_older_female_adult <- hm(non_spatial_oaf, "Older Female Adult Symptom Heatmap", "fig-oaf-heatmap")

# Function for creating rates for aggregated results
symptom_rate <- function(
        age_sex_agg,
        all_agg,
        deaths,
        symptoms,
        malaria_agg,
        infection_agg,
        non_infection_agg){
    
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
    
    # Add all deaths to malaria table
    spatial$malaria <- malaria_agg$malaria_deaths
    spatial$infections <- infection_agg$infection_deaths
    spatial$non_infections <- non_infection_agg$non_infection_deaths
    spatial$all_deaths <- adult_agg$all_deaths
    
    # List of causes of death
    all_deaths <- c("malaria","infections", "non_infections")
    
    # Create rate columns for malaria symptoms
    for (agg_deaths in all_deaths) {
        for (symptom in symptoms) {
            rate_column <- paste0(symptom, "_", agg_deaths, "_rate")
            spatial[[rate_column]] <- (spatial[[symptom]] / spatial[[agg_deaths]]) * 1000
            spatial[[rate_column]] <- round(spatial[[rate_column]], 2)
        }
    }
    
    # Print the wide format
    cat("\nWide format:\n")
    print(spatial)
    
    # Convert spatial to an sf and reproject crs
    spatial <- spatial %>% st_as_sf(sf_column_name = "geometry") %>% st_transform(32628)
    
    # Pivoted spatial table to show rates for each symptom
    out <- spatial %>% 
        pivot_longer(cols = ends_with("rate"),
                     names_to = "symptoms", 
                     values_to = "rates") %>% 
        select(gid, symptoms, rates) %>%
        mutate(symptoms = str_remove(symptoms, "_rate$")) %>% 
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
yam_symptom <- symptom_rate(age_sex_agg = young_male_adult_agg,
                            all_agg = adult_agg, deaths = "deaths",
                            symptoms = adult_symptoms, malaria_agg = adult_malaria_agg,
                            infection_agg = adult_infection_agg, non_infection_agg = adult_non_infection_agg)

yaf_symptom <- symptom_rate(age_sex_agg = young_female_adult_agg,
                            all_agg = adult_agg, deaths = "deaths",
                            symptoms = adult_symptoms, malaria_agg = adult_malaria_agg,
                            infection_agg = adult_infection_agg, non_infection_agg = adult_non_infection_agg)

oam_symptom <- symptom_rate(age_sex_agg = older_male_adult_agg,
                            all_agg = adult_agg, deaths = "deaths",
                            symptoms = adult_symptoms, malaria_agg = adult_malaria_agg,
                            infection_agg = adult_infection_agg, non_infection_agg = adult_non_infection_agg)

oaf_symptom <- symptom_rate(age_sex_agg = older_female_adult_agg,
                            all_agg = adult_agg, deaths = "deaths",
                            symptoms = adult_symptoms, malaria_agg = adult_malaria_agg,
                            infection_agg = adult_infection_agg, non_infection_agg = adult_non_infection_agg)

young_adult_symptom <- symptom_rate(age_sex_agg = young_adult_agg,
                                    all_agg = adult_agg, deaths = "deaths",
                                    symptoms = adult_symptoms, malaria_agg = adult_malaria_agg,
                                    infection_agg = adult_infection_agg, non_infection_agg = adult_non_infection_agg)

older_adult_symptom <- symptom_rate(age_sex_agg = older_adult_agg,
                                    all_agg = adult_agg, deaths = "deaths",
                                    symptoms = adult_symptoms, malaria_agg = adult_malaria_agg,
                                    infection_agg = adult_infection_agg, non_infection_agg = adult_non_infection_agg)

# Creating mappping parameters
create_map <- function(data, symptom, y_axis) {
    filtered_data <- data %>% filter(symptoms == symptom)
    
    breaks_data <- filtered_data %>% mutate(rates = cut(rates, breaks = 6))
    
    if (symptom == "fever") {
        
        map <- ggplot(data = breaks_data) +
            geom_sf(aes(fill=(rates))) +
            guides(fill = guide_legend()) +
            scale_fill_discrete(low="lightblue", high="darkblue", ) +
            ggtitle(paste(symptom)) +
            geom_sf_label(aes(label = rates), size = 1.8) +
            theme_minimal() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.title = element_blank(),
                  axis.text = element_blank(), 
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(angle = 0, vjust = 0.5, size = 20),
                  plot.title = element_text(hjust = 0.5, size = 20)) +
            ylab(y_axis)
    } else {
        map <- ggplot(data = breaks_data) +
            geom_sf(aes(fill=(rates))) +
            guides(fill = guide_legend()) +
            scale_fill_continuous(low="lightblue", high="darkblue", breaks = breaks) +
            ggtitle(paste(symptom)) +
            geom_sf_label(aes(label = rates), size = 1.8) +
            theme_minimal() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.title = element_blank(),
                  axis.text = element_blank(), 
                  axis.ticks = element_blank(), 
                  axis.title = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 20))
        
    }
    return(map)
    
}

create_map_2 <- function(data, symptom, y_axis) {
    filtered_data <- data %>% filter(symptoms == symptom)
    
    breaks_data <- filtered_data %>% mutate(rates = cut(rates, breaks = 6))
    
    if (symptom == "fever") {
        map <- ggplot(data = breaks_data) +
            geom_sf(aes(fill=(rates))) +
            guides(fill = guide_legend()) +
            scale_fill_continuous(low="lightblue", high="darkblue", breaks = breaks) +
            geom_sf_label(aes(label = rates), size = 1.8) +
            theme_minimal() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.title = element_blank(),
                  axis.text = element_blank(), 
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(angle = 0, vjust = 0.5, size = 20),
                  plot.title = element_text(hjust = 0.5, size = 20)) +
            ylab(y_axis)
    } else {
        map <- ggplot(data = breaks_data) +
            geom_sf(aes(fill=(rates))) +
            guides(fill = guide_legend()) +
            scale_fill_continuous(low="lightblue", high="darkblue", breaks = breaks) +
            geom_sf_label(aes(label = rates), size = 1.8) +
            theme_minimal() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.title = element_blank(),
                  axis.text = element_blank(), 
                  axis.ticks = element_blank(), 
                  axis.title = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 20))
    }
    
    return(map)
}

# Creating grouped plots parameters
create_plots <- function(group_symptoms, plot_title, pdf_title) {
    
    malaria_spatial <- group_symptoms %>% filter(denom_group == "Malaria")
    infections_spatial <- group_symptoms %>% filter(denom_group == "Infections")
    non_infections_spatial <- group_symptoms %>% filter(denom_group == "Non-Infections")
    
    symptoms <- unique(group_symptoms$symptoms)
    
    malaria_plots <- lapply(symptoms, create_map, data = malaria_spatial, y_axis = "Malaria")
    infection_plots <- lapply(symptoms, create_map_2, data = infections_spatial, y_axis = "Infections")
    non_infection_plots <- lapply(symptoms, create_map_2, data = non_infections_spatial, y_axis = "Non-Infections")
    
    all_plots <- c(malaria_plots, infection_plots, non_infection_plots)
    
    combined_plot <- wrap_plots(all_plots, ncol = length(malaria_plots)) + 
        plot_annotation(title = plot_title,
                        theme = theme(
                            plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
                        ))
    
    out <- pdf_print(combined_plot, pdf_title)
    
    return(out)
}

# Creating plot series for each age group
yam_plot <- create_plots(yam_symptom, "Young Adult Male Malaria Symptoms", "fig-yam-malaria-maps")
yaf_plot <- create_plots(yaf_symptom, "Young Adult Female Malaria Symptoms", "fig-yaf-malaria-maps")
oam_plot <- create_plots(oam_symptom, "Older Adult Male Malaria Symptoms", "fig-oam-malaria-maps")
oaf_plot <- create_plots(oaf_symptom, "Older Adult Female Malaria Symptoms", "fig-oaf-malaria-maps")
young_adult_plot <- create_plots(young_adult_symptom, "Young Adult Malaria Symptoms", "fig-ya-malaria-maps")
older_adult_plot <- create_plots(older_adult_symptom, "Older Adult Malaria Symptoms", "fig-oa-malaria-maps")