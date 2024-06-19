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
young_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% young_adult_age & cghr10_title == "Malaria")
young_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% young_adult_age & cghr10_title == "Malaria")

# Creating filters for older adults by sex, age, and malaria
older_male_adult <- adult %>% filter(sex_death == "Male" & death_age_group %in% older_adult_age & cghr10_title == "Malaria")
older_female_adult <- adult %>% filter(sex_death == "Female" & death_age_group %in% older_adult_age & cghr10_title == "Malaria")

# Dataframe without malaria deaths
adult_non_malaria <- adult %>% filter(cghr10_title != "Malaria")

# Set mapping dataframe
mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10", "symp11", 
               "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count") 
)

# Testing out function with adult malaria
young_male_adult_malaria <- spatial_agg(gdf = dist,
                                 agg = young_male_adult,
                                 mapping = mapping,
                                 gdf_id = "distname", 
                                 agg_id = "district_cod",
                                 is_spatial_join = FALSE,
                                 count_col = "malaria_deaths")

young_female_adult_malaria <- spatial_agg(gdf = dist,
                                        agg = young_female_adult,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "malaria_deaths")

older_male_adult_malaria <- spatial_agg(gdf = dist,
                                        agg = older_male_adult,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "malaria_deaths")

older_female_adult_malaria <- spatial_agg(gdf = dist,
                                        agg = older_female_adult,
                                        mapping = mapping,
                                        gdf_id = "distname", 
                                        agg_id = "district_cod",
                                        is_spatial_join = FALSE,
                                        count_col = "malaria_deaths")

adult_agg <- spatial_agg(gdf = dist,
                         agg = adult,
                         mapping = mapping,
                         gdf_id = "distname", 
                         agg_id = "district_cod",
                         is_spatial_join = FALSE,
                         count_col = "all_deaths")

# Function for creating rates for aggregated results
adult_symptom_rate <- function(
        age_sex_agg,
        all_agg,
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

# Add all deaths to malaria table
spatial$all_deaths <- all_agg$all_deaths

# Create rate columns for malaria symptoms
spatial$yellowEyes_rate <- (spatial$yellowEyes/spatial$all_deaths) * 1000 
spatial$cough_rate <- (spatial$cough/spatial$all_deaths) * 1000
spatial$vomit_rate <- (spatial$vomit/spatial$all_deaths) * 1000
spatial$breathingProblem_rate <- (spatial$breathingProblem/spatial$all_deaths) * 1000
spatial$abdominalProblem_rate <- (spatial$abdominalProblem/spatial$all_deaths) * 1000
spatial$fever_rate <- (spatial$fever/spatial$all_deaths) * 1000

# Round to 2 decimal places
spatial <- spatial %>% mutate(yellowEyes_rate = round(yellowEyes_rate, 2))
spatial <- spatial %>% mutate(cough_rate = round(cough_rate, 2))
spatial <- spatial %>% mutate(vomit_rate = round(vomit_rate, 2))
spatial <- spatial %>% mutate(breathingProblem_rate = round(breathingProblem_rate, 2))
spatial <- spatial %>% mutate(abdominalProblem_rate = round(abdominalProblem_rate, 2))
spatial <- spatial %>% mutate(fever_rate = round(fever_rate, 2))

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
    mutate(symptoms = str_remove(symptoms, "_rate$"))

return(out)

}

# Running symptom_rate for each age group
yam_symptom <- adult_symptom_rate(age_sex_agg = young_male_adult_malaria,
                            all_agg = adult_agg, deaths = "malaria_deaths")
yaf_symptom <- adult_symptom_rate(age_sex_agg = young_female_adult_malaria,
                            all_agg = adult_agg, deaths = "malaria_deaths")
oam_symptom <- adult_symptom_rate(age_sex_agg = older_male_adult_malaria,
                            all_agg = adult_agg, deaths = "malaria_deaths")
oaf_symptom <- adult_symptom_rate(age_sex_agg = older_female_adult_malaria,
                            all_agg = adult_agg, deaths = "malaria_deaths")

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
non_spatial_adult <- non_spatial %>% left_join(death_count, by = "cghr10_title")

colnames(non_spatial_adult)[colnames(non_spatial_adult) == "cghr10_title"] <- "cause_of_death"

# Creating mappping parameters
create_map <- function(data, symptom) {
    filtered_data <- data %>% filter(symptoms == symptom)
    ggplot(data = filtered_data) +
    geom_sf(aes(fill=(rates))) +
    guides(fill = guide_legend(title = "Cases per 1000 deaths")) +
    scale_fill_continuous(low="lightblue", high="darkblue") +
    annotation_north_arrow(width = unit(0.4, "cm"),height = unit(0.5, "cm"), location = "tr") +
    annotation_scale(plot_unit = "m", style = "ticks", location = "bl") +
    ggtitle(paste(symptom)) +
    geom_sf_label(aes(label = rates), size = 1.8) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5))
}

# Creating grouped plots parameters
create_plots <- function(group_symptoms, plot_title) {
    
    symptoms <- unique(group_symptoms$symptoms)
    
    plots <- lapply(symptoms, create_map, data = group_symptoms)
    
    combined_plot <- wrap_plots(plots) + plot_annotation(title = plot_title)
    
    return(combined_plot)
}

# Creating plot series for each age group
yam_plot <- create_plots(yam_symptom, "Young Adult Male Malaria Symptoms")
yaf_plot <- create_plots(yaf_symptom, "Young Adult Female Malaria Symptoms")
oam_plot <- create_plots(oam_symptom, "Older Adult Male Malaria Symptoms")
oaf_plot <- create_plots(oaf_symptom, "Older Adult Female Malaria Symptoms")

# Viewing plots for each map series
yam_plot
yaf_plot
oam_plot
oaf_plot

# Creating PDF export parameters
pdf_print <- function(series, title){
    
    output_dir <- "C:/Users/dante/msa-2024-internship/figures/"
    
    pdf_title <- paste0(output_dir, title, ".pdf")
    
    out = ggsave(pdf_title, plot = series, device = "pdf", width = 14, height = 8)
    
    return(out)
    
}

# Exporting plot series as PDFs
yam_pdf <- pdf_print(yam_plot, "fig-yam-malaria-maps")
yaf_pdf <- pdf_print(yaf_plot, "fig-yaf-malaria-maps")
oam_pdf <- pdf_print(oam_plot, "fig-oam-malaria-maps")
oaf_pdf <- pdf_print(oaf_plot, "fig-oaf-malaria-maps")


# Creating heat map with non-spatial table
heat <- pivot_longer(non_spatial_adult, cols = -cause_of_death,
                     names_to = "symptoms",
                     values_to = "rates") %>%
    filter(cause_of_death != "NA" & symptoms != "NA")

heat_map_adult <- ggplot(heat, aes(symptoms, cause_of_death)) +
    geom_tile(aes(fill = rates)) +
    geom_text(aes(label = round(rates, 1))) +
    scale_fill_gradient(low = "white", high = "red") +
    theme(axis.text.x = element_text(size = 3))

# Viewing plot of heat map
heat_map_adult

# Exporting heat map as pdf
hm_adult <- pdf_print(heat_map_adult, "Adult Heatmap")