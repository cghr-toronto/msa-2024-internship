library(dplyr)
library(magrittr)
library(sf)
library(tidyverse)

df <- iris

iris_data <- data.frame(
  column = c("Sepal.Length", "Sepal.Width"),
  can_aggregate = c("sum, mean", "mean, median") 
)

agg_funcs <- c("mean", "sum", "median")

iris_funcs <- list()

sum_func <- list()

for (func_name in agg_funcs) {
  
  if (func_name %in% c("median", "mean", "sum")){
    
    # Fill your list where each key is func and each value is the columns having the relevant aggregate function
    iris_funcs[[func_name]] <- iris_data %>%
      filter(str_detect(can_aggregate, func_name)) %>%
      pull(column)
    
    func <- get(func_name)
    
    sum_func[[func_name]] <- summarise_at(iris, iris_funcs[[func_name]], func) %>%
      rename_with(
        .tbl = .df,
        .fn = ~ paste0("prefix_", .x, recycle0 = TRUE),
        .cols = starts_with(func_name))
    
  }
}

combined_results <- bind_cols(sum_func)
