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
dist <- st_read("dist.csv")

all_ages <- bind_rows(adult_table, child_table)

all_ages_malaria <- all_ages %>% filter(type_of_cause == "Malaria")
all_ages_infections <- all_ages %>% filter(type_of_cause == "Infections")
all_ages_non_infections <- all_ages %>% filter(type_of_cause == "Non-infections")

non_spatial_all_ages <- non_spatial(age_group = all_ages, death_type = "type_of_cause", percentages = FALSE)

cod_custom_order <- c("Malaria", "Infections", "Non-infections")

all_ages_symptoms <- c("fever", "abdominalProblem", "breathingProblem", "cough", "vomit", "weightLoss", 
                    "chestPain", "unconscious", "paralysis", "looseStools", "urinaryProblem", "oedema", 
                    "skinProblems", "yellowEyes", "convulsions", "lumps")

hm_adult <-
    hm(
        non_spatial_adult,
        "All Deaths by Symptom\nSierra Leone 2019-2022",
        "fig-all-heatmap",
        labels = TRUE,
        cod_order = "manual",
        cod_custom_order = cod_custom_order,
        symp_custom_order = adult_symptoms,
        keep_only = TRUE,
        symptoms = adult_symptoms,
        width = 12,
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