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
male_all_ages <- all_ages %>% filter(sex_death == "Male")
female_all_ages <- all_ages %>% filter(sex_death == "Female")

all_ages_malaria <- all_ages %>% filter(type_of_cause == "Malaria")
all_ages_infections <- all_ages %>% filter(type_of_cause == "Infections")
all_ages_non_infections <- all_ages %>% filter(type_of_cause == "Non-infections")

male_malaria <- male_all_ages %>% filter(type_of_cause == "Malaria")
male_infections <- male_all_ages %>% filter(type_of_cause == "Infections")
male_non_infections <- male_all_ages %>% filter(type_of_cause == "Non-infections")

female_malaria <- female_all_ages %>% filter(type_of_cause == "Malaria")
female_infections <- female_all_ages %>% filter(type_of_cause == "Infections")
female_non_infections <- female_all_ages %>% filter(type_of_cause == "Non-infections")

non_spatial_all_ages <- non_spatial(age_group = all_ages, death_type = "type_of_cause", percentages = FALSE)
non_spatial_male <- non_spatial(age_group = male_all_ages, death_type = "type_of_cause", percentages = FALSE)
non_spatial_female <- non_spatial(age_group = female_all_ages, death_type = "type_of_cause", percentages = FALSE)

cod_custom_order <- c("Malaria", "Infections", "Non-infections")

all_ages_symptoms <- c("breathingProblem", "cough", "fever",
                       "vomit", "weightLoss","yellowEyes")

# hm_male <-
#     hm(
#         non_spatial_male,
#         "All Male Deaths by Symptom\nSierra Leone 2019-2022",
#         "fig-male-heatmap",
#         labels = TRUE,
#         cod_order = "manual",
#         cod_custom_order = cod_custom_order,
#         symp_custom_order = all_ages_symptoms,
#         keep_only = TRUE,
#         symptoms = all_ages_symptoms,
#         width = 9,
#         height = 25
#     )
# 
# hm_female <-
#     hm(
#         non_spatial_female,
#         "All Female Deaths by Symptom\nSierra Leone 2019-2022",
#         "fig-female-heatmap",
#         labels = TRUE,
#         cod_order = "manual",
#         cod_custom_order = cod_custom_order,
#         symp_custom_order = all_ages_symptoms,
#         keep_only = TRUE,
#         symptoms = all_ages_symptoms,
#         width = 9,
#         height = 25
#     )

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
        height = 25
    )

mapping <- data.frame(
    column = c("symp1", "symp2", "symp3", "symp4", "symp5", "symp6", "symp7", "symp8", "symp9", "symp10", "symp11", 
               "symp12", "symp13", "symp14"),
    can_aggregate = c("count", "count", "count", "count", "count", "count", "count", "count", "count", "count", "count", 
                      "count", "count", "count") 
)

male_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = male_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

male_infections_agg <- spatial_agg(
    gdf = dist,
    agg = male_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

male_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = male_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

female_malaria_agg <- spatial_agg(
    gdf = dist,
    agg = female_malaria,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

female_infections_agg <- spatial_agg(
    gdf = dist,
    agg = female_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
)

female_non_infections_agg <- spatial_agg(
    gdf = dist,
    agg = female_non_infections,
    mapping = mapping,
    gdf_id = "distname",
    agg_id = "district_cod",
    is_spatial_join = FALSE,
    count_col = "deaths"
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

male_malaria_symptom <- symptom_rate(age_sex_agg = male_malaria_agg,
                                         cod = "malaria",
                                         deaths = "deaths",
                                         symptoms = all_ages_symptoms)
male_infections_symptom <- symptom_rate(age_sex_agg = male_infections_agg,
                                            cod = "infections",
                                            deaths = "deaths",
                                            symptoms = all_ages_symptoms)
male_non_infections_symptom <- symptom_rate(age_sex_agg = male_non_infections_agg,
                                                cod = "non_infections",
                                                deaths = "deaths",
                                                symptoms = all_ages_symptoms)
male_symptom <- bind_rows(male_malaria_symptom, male_infections_symptom, male_non_infections_symptom)
male_symptom <- male_symptom %>% mutate(
    age_range = "<1-69",
    age_group = "All",
    sex = "Male")

female_malaria_symptom <- symptom_rate(age_sex_agg = female_malaria_agg,
                                     cod = "malaria",
                                     deaths = "deaths",
                                     symptoms = all_ages_symptoms)
female_infections_symptom <- symptom_rate(age_sex_agg = female_infections_agg,
                                        cod = "infections",
                                        deaths = "deaths",
                                        symptoms = all_ages_symptoms)
female_non_infections_symptom <- symptom_rate(age_sex_agg = female_non_infections_agg,
                                            cod = "non_infections",
                                            deaths = "deaths",
                                            symptoms = all_ages_symptoms)
female_symptom <- bind_rows(female_malaria_symptom, female_infections_symptom, female_non_infections_symptom)
female_symptom <- female_symptom %>% mutate(
    age_range = "<1-69",
    age_group = "All",
    sex = "Female")

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

symptom_rate_tables <- c("all_ages_symptom", "male_symptom", "female_symptom")

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

male_plot <-
    create_plots(
        male_symptom,
        "All Male Deaths by Symptom\nSierra Leone, 2019-2022",
        "fig-male-malaria-maps",
        width = 12,
        height = 25,
        age_range = "<1-69",
        age_group = "All",
        sex = "Male",
        orientation = "portrait",
        symptom_order = all_ages_symptoms
    )

female_plot <-
    create_plots(
        female_symptom,
        "All Female Deaths by Symptom\nSierra Leone, 2019-2022",
        "fig-female-malaria-maps",
        width = 12,
        height = 25,
        age_range = "<1-69",
        age_group = "All",
        sex = "Female",
        orientation = "portrait",
        symptom_order = all_ages_symptoms
    )

all_ages_plot <-
    create_plots(
        all_ages_symptom,
        "All Deaths by Symptom\nSierra Leone, 2019-2022",
        "fig-all-ages-malaria-maps",
        width = 12,
        height = 25,
        age_range = "<1-69",
        age_group = "All",
        sex = "Both",
        orientation = "portrait",
        symptom_order = all_ages_symptoms
    )