library(dplyr)
library(magrittr)
library(sf)
library(tidyverse)
library(ggplot2)


df <- iris

iris_data <- data.frame(
  column = c("Sepal.Length", "Sepal.Width"),
  can_aggregate = c("sum, mean", "mean, count") 
)

agg_funcs <- c("mean", "sum", "count")


iris_funcs <- list()

for (func in agg_funcs) {
  iris_cols <- iris_data %>%
    filter(str_detect(can_aggregate, func)) %>%
    pull(column)
  
  if (func %in% c("count")) {
    sum_fun <- df %>%
      summarise(across(all_of(iris_cols), .fns = n))
  } else {
    func_name <- get(func)
    sum_fun <- df %>%
      summarise(across(all_of(iris_cols), .fns = func_name))
  }
  
  iris_funcs[[func]] <- sum_fun
}
