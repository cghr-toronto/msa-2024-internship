library(dplyr)
library(magrittr)
library(sf)
library(tidyverse)
library(ggplot2)


df <- iris

iris_data <- data.frame(
  column = c("Sepal.Length", "Sepal.Width"),
  can_aggregate = c("mean", "mean") 
)

agg_funcs <- c("mean", "sum", "count")


iris_funcs <- list()

for (func in agg_funcs) {
  # Fill your list where each key is func and each value is the columns having the relevant aggregate function
  iris_funcs[[func]] <- iris_data %>%
    filter(str_detect(can_aggregate, func), column == "Sepal.Width") %>%
    pull(column)
}

iris_funcs[["mean"]]