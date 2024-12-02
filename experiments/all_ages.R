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

adult_table <- st_read("adult.csv")
child_table <- st_read("child.csv")
# Reading District Boundary file
dist <- st_read("../tmp/data/sl_dist_17_v2.geojson")

all_ages <- bind_rows(adult_table, child_table)

all_ages_malaria <- all_ages %>% filter(type_of_cause == "Malaria")
all_ages_infections <- all_ages %>% filter(type_of_cause == "Infections")
all_ages_non_infections <- all_ages %>% filter(type_of_cause == "Non-infections")

non_spatial_all_ages <- non_spatial(age_group = all_ages, death_type = "type_of_cause", percentages = FALSE)

cod_custom_order <- c("Malaria", "Infections", "Non-infections")

all_ages_symptoms <- c("fever", "abdominalProblem", "breathingProblem", "cough", "vomit", "weightLoss", 
                    "chestPain", "unconscious", "paralysis", "looseStools", "urinaryProblem", "oedema", 
                    "skinProblems", "yellowEyes", "convulsions", "lumps")

hm_all_ages <-
    hm(
        non_spatial_all_ages,
        "All Deaths by Symptom\nSierra Leone 2019-2022",
        "fig-all-ages-heatmap",
        labels = TRUE,
        cod_order = "manual",
        cod_custom_order = cod_custom_order,
        symp_custom_order = all_ages_symptoms,
        keep_only = TRUE,
        symptoms = all_ages_symptoms,
        width = 9,
        height = 42
    )

mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10", "symp11", 
               "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count") 
)

all_ages_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = all_ages_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

all_ages_infections_agg <- spatial_agg(
    gdf = dist,
    agg = all_ages_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

all_ages_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = all_ages_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

all_ages_malaria_symptom <- symptom_rate(age_sex_agg = all_ages_malaria_agg,
                                      cod = "malaria",
                                      deaths = "deaths",
                                      symptoms = all_ages_symptoms)
all_ages_infections_symptom <- symptom_rate(age_sex_agg = all_ages_infections_agg,
                                         cod = "infections",
                                         deaths = "deaths",
                                         symptoms = all_ages_symptoms)
all_ages_non_infections_symptom <- symptom_rate(age_sex_agg = all_ages_non_infections_agg,
                                             cod = "non_infections",
                                             deaths = "deaths",
                                             symptoms = all_ages_symptoms)
all_ages_symptom <- bind_rows(all_ages_malaria_symptom, all_ages_infections_symptom, all_ages_non_infections_symptom)
all_ages_symptom <- all_ages_symptom %>% mutate(
    age_range = "<1-69",
    age_group = "All",
    sex = "Both")

symptom_rate_tables <- c("all_ages_symptom")

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

all_ages_plot <-
    create_plots(
        all_ages_symptom,
        "All Deaths by Symptom\nSierra Leone, 2019-2022",
        "fig-all-ages-malaria-maps",
        width = 12,
        height = 42,
        age_range = "<1-69",
        age_group = "All",
        sex = "Both",
        orientation = "portrait",
        symptom_order = all_ages_symptoms
    )