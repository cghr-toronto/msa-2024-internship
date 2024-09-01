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

limits <- c(min_val, max_val)

map <- ggplot(data = filtered_data) +
    geom_sf(aes(fill = rates), color = "gray50", size = 0.2) +
    scale_fill_gradientn(colors = c("white",
                                    "lightgreen",
                                    "lawngreen",
                                    "forestgreen",
                                    "darkgreen",
                                    "gold",
                                    "orange",
                                    "orangered",
                                    "red",
                                    "darkred"),
                         values = scales::rescale(c(0, 10, 40, 50, 60, 70, 80, 90, 100)),
                         na.value = "white",  # Handle NA values
                         breaks = break_points,
                         labels = label,
                         limits = limits) +
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
