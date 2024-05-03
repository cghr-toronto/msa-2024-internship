library(dplyr)
library(magrittr)
library(sf)
library(tidyverse)



df <- iris

iris_data <- data.frame(
  column = c("Sepal.Length", "Sepal.Width"),
  can_aggregate = c("sum, mean", "mean, count") 
)

agg_funcs <- c("mean", "sum", "count")


iris_funcs <- list()

for (func in agg_funcs) {
  
  if (func %in% c("count", "mean", "sum")){
    
    # Fill your list where each key is func and each value is the columns having the relevant aggregate function
    iris_funcs[[func]] <- iris_data %>%
      filter(str_detect(can_aggregate, func)) %>%
      pull(column)
    
    func_name <- get(func)
    
    sum_fun <- summarise_at(iris, iris_funcs[[func]], func_name)
  }
}