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

min_val <- min(data$rates, na.rm = TRUE)
max_val <- max(data$rates, na.rm = TRUE)

limits <- c(min_val, max_val)

break_points <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

label <-
    c("Insufficient Data",
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

filtered_data$legend_label <- factor(sapply(filtered_data$rates, categorize_value),
                                     levels = label)

map <- ggplot(data = filtered_data) +
    geom_sf(aes(fill = legend_label), color = "gray50", size = 0.2) +
    scale_fill_manual(breaks = label,
                      labels = label,
                      values = c("Insufficient Data" = "white",
                                 "0-10" = "lightgreen",
                                 "10-20" = "green",
                                 "20-30" = "forestgreen",
                                 "30-40" = "darkgreen",
                                 "40-50" = "yellow",
                                 "50-60" = "gold",
                                 "60-70" = "orange",
                                 "70-80" = "orangered",
                                 "80-90" = "red",
                                 "90-100" = "darkred"),
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

map <- map + ylab("fever")
