library(dplyr)
library(magrittr)

df <- iris

df %>% filter(Petal.Width == 0.2) %>% summarise_at(vars(Species), length)



