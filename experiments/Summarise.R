library(dplyr)
library(magrittr)


df <- iris

df %>% summarise_all(mean, na.rm = TRUE)
