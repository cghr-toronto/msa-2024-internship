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



data <- data %>%
    mutate(
        data_quality = ifelse(deaths < 10, "Insufficient Data", "Sufficient Data"),
        rates = ifelse(data_quality == "Insufficient Data", NA, rates)
    )

filtered_data <- data %>%
    filter(symptoms == symptom & denom_group == cod) %>%
    mutate(fraction = glue("{count}/{deaths}"))

filtered_data$rates <- as.factor(filtered_data$rates)

map <- ggplot(data = filtered_data) +
    geom_sf(aes(fill = rates), color = "gray50", size = 0.2) +
    scale_fill_manual(
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
        ),
        breaks = c(
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
        ),
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
    ) +
    guides(fill = guide_legend(nrow = 1, title = "Rates (%)")) +
    ggtitle(gplot_title) +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = if (y_axis)
            element_text(
                angle = 0,
                vjust = 0.5,
                size = 20
            )
        else
            element_blank(),
        plot.title = if (first_map == symptom)
            element_text(hjust = 0.5, size = 17)
        else
            element_blank()
    ) +
    ylab(paste(symptom))
}


map <-
    map + geom_sf_label(aes(label = fraction), size = 1.8)

map <- map + ylab(paste(symptom))
