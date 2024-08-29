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
library(scales)

group_symptoms <- all_adult_symptom %>% filter(age_range == "15-69" & age_group == "Adult" & sex == "Both")

symptoms <- unique(group_symptoms$symptoms)

fm <- symptoms[[1]]

data <- group_symptoms %>%
    mutate(
        data_quality = ifelse(deaths < 10, "Insufficient Data", "Sufficient Data"),
        rates = ifelse(data_quality == "Insufficient Data", NA, rates)
    )

filtered_data <- data %>%
    filter(symptoms == "fever" & denom_group == "Malaria") %>%
    mutate(fraction = glue("{count}/{deaths}"))

# Define a function to categorize values
categorize_value <- function(value) {
    if (is.na(value)) {
        return("Insufficient Data")
    } else if (value < 10) {
        return("0-10")
    } else if (value >= 10 & value < 20) {
        return("10-20")
    } else if (value >= 20 & value < 30) {
        return("20-30")
    } else if (value >= 30 & value < 40) {
        return("30-40")
    } else if (value >= 40 & value < 50) {
        return("40-50")
    } else if (value >= 50 & value < 60) {
        return("50-60")
    } else if (value >= 60 & value < 70) {
        return("60-70")
    } else if (value >= 70 & value < 80) {
        return("70-80")
    } else if (value >= 80 & value < 90) {
        return("80-90")
    } else if (value >= 90 & value <= 100) {
        return("90-100")
    } else {
        return(NA)
    }
}

filtered_data$legend_label <- sapply(filtered_data$rates, categorize_value)

values = c(
    "Insufficient Data" = "white",
    "0-10" = "lightgreen",
    "10-20" = "green",
    "20-30" = "darkgreen",
    "30-40" = "yellow",
    "40-50" = "gold",
    "50-60" = "orange",
    "60-70" = "red",
    "70-80" = "darkred",
    "80-90" = "purple",
    "90-100" = "black"
)

labels = c(
    "Insufficient Data",
    "0-10",
    "10-20",
    "20-30",
    "30-40",
    "40-50",
    "50-60",
    "60-70",
    "70-80",
    "80-90",
    "90-100"
)

filtered_data$legend_label <- factor(filtered_data$legend_label, levels = labels)

dummy_data <- data.frame(
    legend_label = factor(labels, levels = labels),
    fill = NA  # No actual data, just to ensure all levels are displayed
)

bbox <- st_bbox(filtered_data)  # Get the bounding box of actual data

# Create dummy data with arbitrary coordinates
dummy_data <- data.frame(
    lon = rep(mean(bbox["xmin"]), length(labels)),  # Arbitrary lon values
    lat = rep(mean(bbox["ymin"]), length(labels)),  # Arbitrary lat values
    legend_label = factor(labels, levels = labels)  # Ensure all labels are included
)

# Convert dummy data to sf object
dummy_data_sf <- st_as_sf(dummy_data, coords = c("lon", "lat"), crs = st_crs(filtered_data))

missing_columns <- setdiff(names(filtered_data), names(dummy_data_sf))
for (col in missing_columns) {
    dummy_data_sf[[col]] <- NA
}

# Ensure the same column names and types in both data frames
dummy_data_sf <- dummy_data_sf %>% 
    select(all_of(names(filtered_data)))

# Combine the original and dummy data
combined_data <- rbind(filtered_data, dummy_data_sf) 

map <- ggplot(data = filtered_data) +
    geom_sf(aes(fill = legend_label)) +
    scale_fill_manual(values = values,
                      breaks = labels,
                      labels = labels,
                      limits = limits(c(0, 100)),
                      drop = FALSE) +
    guides(fill = guide_legend(nrow = 1, title = "Rates (%)")) +
    ggtitle("Cases\nper 100\nMalaria deaths") +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = 
            element_text(
                angle = 0,
                vjust = 0.5,
                size = 20)
    ) +
    ylab("fever")


map <-
    map + geom_sf_label(aes(label = fraction), size = 1.8)

map <- map + ylab(paste(symptom))
