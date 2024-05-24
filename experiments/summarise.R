library(dplyr)
library(magrittr)
library(sf)
library(tidyverse)

df <- iris

mappings <- data.frame(
  column = c("Sepal.Length", "Sepal.Width"),
  can_aggregate = c("sum, mean", "mean, median") 
)

agg_funcs <- c("mean", "sum", "median")

df_funcs <- list()

sum_func <- list()

for (func_name in agg_funcs) {
  
  if (func_name %in% c("median", "mean", "sum")){
    
    # Get cols
    df_funcs[[func_name]] <- mappings %>%
      filter(str_detect(can_aggregate, func_name)) %>%
      pull(column)
    
    # Get
    func <- get(func_name)
    
    # Apply
    sum_func[[func_name]] <- summarise_at(df, df_funcs[[func_name]], func) %>%
      rename_with(
        .fn = ~ paste0(func_name, "_", .),
        .cols = everything()
      )
    
  }
}

combined_results <- bind_cols(sum_func)
