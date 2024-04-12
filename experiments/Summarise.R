p_w <- c("0.2", "0.3", "0,5")

p_w_list <- list()

for (func in p_w) {
  # Fill your list where each key is func and each value is the columns having the relevant aggregate function
  p_w_list[[func]] <- iris %>%
    filter(str_detect(Petal.Width, func)) %>%
    pull(Species)
}