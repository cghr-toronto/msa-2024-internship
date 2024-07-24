source("../src/spatial_agg.R")

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

# Creating PDF export parameters
pdf_print <- function(series, title){
    
    pdf_output_dir <- "../figures/"
    
    jpeg_output_dir <- "../figures.jpgs/"
    
    pdf_title <- paste0(pdf_output_dir, title, ".pdf")
    
    jpeg_title <- paste0(jpeg_output_dir, title, ".jpeg")
    
    ggsave(pdf_title, plot = series, device = "pdf", width = 26, height = 13)
    
    ggsave(jpeg_title, plot = series, device = "jpeg", width = 26, height = 13)
    
}

pdf_print_hm <- function(series, title){
    
    pdf_output_dir <- "../figures/"
    
    jpeg_output_dir <- "../figures.jpgs/"
    
    pdf_title <- paste0(pdf_output_dir, title, ".pdf")
    
    jpeg_title <- paste0(jpeg_output_dir, title, ".jpeg")
    
    ggsave(pdf_title, plot = series, device = "pdf", width = 6, height = 24)
    
    ggsave(jpeg_title, plot = series, device = "jpeg", width = 6, height = 24)
    
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

# Test----
total_deaths <- function(ns_table, deaths){
    as.numeric(sum(ns_table$deaths))
}

# Creating heat map with non-spatial table
hm <- function(ns_table, hm_title, pdf_title) {
    
    death_total <- total_deaths(ns_table, deaths) 
    
    malaria <- sum(ns_table$deaths[ns_table$type_of_cause == "Malaria"], na.rm = TRUE)
    infections <- sum(ns_table$deaths[ns_table$type_of_cause == "Infections"], na.rm = TRUE)
    non_infections <- sum(ns_table$deaths[ns_table$type_of_cause == "Non-infections"], na.rm = TRUE)

    heat <- pivot_longer(ns_table, cols = -c(cause_of_death, type_of_cause),
                         names_to = "symptoms",
                         values_to = "counts") %>%
        filter(cause_of_death != "NA" & symptoms != "NA" & symptoms != "deaths") %>% 
        group_by(type_of_cause, symptoms) %>%
        summarise(total_count = sum(counts))
    
    # Calculating sum of each cause of death
    toc_sums <- heat %>%
        group_by(type_of_cause) %>%
        summarize(toc_sum = sum(total_count, na.rm = TRUE))
    
    all_deaths <- sum(toc_sums$toc_sum)
    
    # Calculating sum of each symptom death
    symp_sums <- heat %>%
        group_by(symptoms) %>%
        summarize(symp_sum = sum(total_count, na.rm = TRUE))
    
    # Calculating percentage for symptoms
    symp_sums$symp_perc <- round((symp_sums$symp_sum / death_total) * 100)
    
    # Merge the sums back into the original data frame
    heat <- heat %>%
        left_join(toc_sums, by = "type_of_cause") %>%
        left_join(symp_sums, by = "symptoms")
    
    heat <- heat %>% 
        mutate(total_perc = case_when(
            type_of_cause == "Malaria" ~ round((total_count / malaria) * 100),
            type_of_cause == "Infections" ~ round((total_count / infections) * 100),
            type_of_cause == "Non-infections" ~ round((total_count / non_infections) * 100)
        )) %>%
        mutate(type_of_cause = case_when(
            type_of_cause == "Malaria" ~ glue("Malaria\n(n={malaria})"),
            type_of_cause == "Infections" ~ glue("Infections\n(n={infections})"),
            type_of_cause == "Non-infections" ~ glue("Non-infections\n(n={non_infections})")
        )) %>%
        mutate(symptoms = ifelse(symp_perc < 1,
                                 glue("{symp_sums$symptoms}\n({symp_sums$symp_sum}, <1%)"),
                                 glue("{symp_sums$symptoms}\n({symp_sums$symp_sum}, {symp_sums$symp_perc}%)")
                                 )
               )
    
    heat$symptoms <- fct_reorder(heat$symptoms, heat$symp_sum, .desc = FALSE)
    
    heat$type_of_cause <- factor(heat$type_of_cause, levels = c(glue("Malaria\n(n={malaria})"),
                                                                glue("Infections\n(n={infections})"),
                                                                glue("Non-infections\n(n={non_infections})")))

        
    # Create the heatmap with modified axis labels
    heat_map_plot <- ggplot(heat, aes(type_of_cause, symptoms)) +
        geom_tile(aes(fill = total_count)) +
        geom_text(aes(label = ifelse(total_perc < 1, 
                                     glue("{total_count}\n(<1%)"), 
                                     glue("{total_count}\n({total_perc}%)")
                                     )
                      )) +
        scale_fill_gradient(low = "white", high = "red", name = "Number\nof deaths",) +
        scale_x_discrete(position = "top") +
        ggtitle(glue("{hm_title}\n(n = {death_total})")) +
        theme(axis.text.x = element_text(size = 10.5),
              axis.text.y = element_text(size = 10.5),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              legend.position = "right",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) 
    
    # Exporting heat map as pdf
    out <- pdf_print_hm(heat_map_plot, pdf_title)
    
    return(out)
    
}

hm_adult <- hm(non_spatial_adult, glue("Adult (15-69 Years) Deaths by Symptom\nSierra Leone 2019-2022"), "fig-adult-heatmap")
hm_young_adult <- hm(non_spatial_young_adult, "Young Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-young-adult-heatmap")
hm_older_adult <- hm(non_spatial_older_adult, "Older Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-older-adult-heatmap")
hm_young_male_adult <- hm(non_spatial_yam, "Young Male Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-yam-heatmap")
hm_young_female_adult <- hm(non_spatial_yaf, "Young Female Adult (15-39 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-yaf-heatmap")
hm_older_male_adult <- hm(non_spatial_oam, "Older Male Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-oam-heatmap")
hm_older_female_adult <- hm(non_spatial_oaf, "Older Female Adult (40-69 Years) Deaths by Symptom\nSierra Leone 2019-2022", "fig-oaf-heatmap")

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
    
    browser()
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
    
    out$rates[is.nan(out$rates)] <- 0
    
    return(out)
    
}

# Defining symptoms to be plotted
adult_symptoms <- c("fever", "abdominalProblem", "breathingProblem", "cough", "vomit", "weightLoss")

# Running symptom_rate for each age group
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

# Creating mappping parameters
create_map <- function(data, symptom, y_axis) {
    filtered_data <- data %>% filter(symptoms == symptom)
    
    min_val <- min(filtered_data$rates, na.rm = TRUE)
    max_val <- max(filtered_data$rates, na.rm = TRUE)
    
    breaks <- 6
    
    # Calculate the interval width
    interval_width <- max_val / breaks
    
    # Generate the sequence of break points
    break_points <- seq(min_val, max_val, len = 6)

    if (symptom == "fever") {
        
        map <- ggplot(data = filtered_data) +
            geom_sf(aes(fill=(rates))) +
            guides(fill = guide_legend()) +
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
            ylab(y_axis) +
            scale_fill_continuous(low="lightblue", 
                                  high="darkblue", 
                                  breaks = break_points, 
                                  labels = scales::number_format(accuracy = 1)) 
    } else {
        map <- ggplot(data = filtered_data) +
            geom_sf(aes(fill=(rates))) +
            guides(fill = guide_legend()) +
            ggtitle(paste(symptom)) +
            geom_sf_label(aes(label = rates), size = 1.8) +
            theme_minimal() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.title = element_blank(),
                  axis.text = element_blank(), 
                  axis.ticks = element_blank(), 
                  axis.title = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 20)) +
            scale_fill_continuous(low="lightblue", 
                                  high="darkblue", 
                                  breaks = break_points, 
                                  labels = scales::number_format(accuracy = 1)) 
        
    }
    return(map)
    
}

create_map_2 <- function(data, symptom, y_axis) {
    filtered_data <- data %>% filter(symptoms == symptom)
    
    min_val <- min(filtered_data$rates, na.rm = TRUE)
    max_val <- max(filtered_data$rates, na.rm = TRUE)
    
    breaks <- 6
    
    # Calculate the interval width
    interval_width <- max_val / breaks
    
    # Generate the sequence of break points
    break_points <- seq(min_val, max_val, len = 6)
    
    if (symptom == "fever") {
        map <- ggplot(data = filtered_data) +
            geom_sf(aes(fill=(rates))) +
            guides(fill = guide_legend()) +
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
            ylab(y_axis) +
            scale_fill_continuous(low="lightblue", 
                                  high="darkblue", 
                                  breaks = break_points, 
                                  labels = scales::number_format(accuracy = 1)) 
    } else {
        map <- ggplot(data = filtered_data) +
            geom_sf(aes(fill=(rates))) +
            guides(fill = guide_legend()) +
            geom_sf_label(aes(label = rates), size = 1.8) +
            theme_minimal() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.title = element_blank(),
                  axis.text = element_blank(), 
                  axis.ticks = element_blank(), 
                  axis.title = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 20)) +
            scale_fill_continuous(low="lightblue", 
                                  high="darkblue",
                                  breaks = break_points, 
                                  labels = scales::number_format(accuracy = 1)) 
    }
    
    return(map)
}

# Creating grouped plots parameters
create_plots <- function(group_symptoms, plot_title, pdf_title) {
    
    malaria_spatial <- group_symptoms %>% filter(denom_group == "Malaria")
    infections_spatial <- group_symptoms %>% filter(denom_group == "Infections")
    non_infections_spatial <- group_symptoms %>% filter(denom_group == "Non-Infections")
    
    symptoms <- unique(group_symptoms$symptoms)
    
    malaria_plots <- lapply(symptoms, create_map, data = malaria_spatial, y_axis = "Malaria\n(per 100\nMalaria deaths)")
    infection_plots <- lapply(symptoms, create_map_2, data = infections_spatial, y_axis = "Infections\n(per 100\nInfection deaths)")
    non_infection_plots <- lapply(symptoms, create_map_2, data = non_infections_spatial, y_axis = "Non-Infections\n(per 100\nNon-Infection deaths)")
    
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
yam_plot <- create_plots(yam_symptom, "Young Adult Male (15-39 Years) Malaria Symptoms", "fig-yam-malaria-maps")
yaf_plot <- create_plots(yaf_symptom, "Young Adult Female (15-39 Years) Malaria Symptoms", "fig-yaf-malaria-maps")
oam_plot <- create_plots(oam_symptom, "Older Adult Male (40-69 Years) Malaria Symptoms", "fig-oam-malaria-maps")
oaf_plot <- create_plots(oaf_symptom, "Older Adult Female (40-69 Years) Malaria Symptoms", "fig-oaf-malaria-maps")
young_adult_plot <- create_plots(young_adult_symptom, "Young Adult (15-39 Years) Malaria Symptoms", "fig-ya-malaria-maps")
older_adult_plot <- create_plots(older_adult_symptom, "Older Adult (40-69 Years) Malaria Symptoms", "fig-oa-malaria-maps")

