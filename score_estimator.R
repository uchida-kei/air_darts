invisible({rm(list=ls());gc();gc()})

library(ggplot2)
ggplot() + theme_set(theme_bw(base_size = 14,base_family="HiraKakuProN-W3"))

library(readr)
library(tibble)
library(purrr)
library(dtw)
library(tidyr)
library(dplyr)
library(stringr)

args <- commandArgs(trailingOnly = T)
default_args <- "./data/acc/0001-acc.csv"
args <- ifelse(length(args) == 0, default_args, args)

area_vectors <- 1:6 %>% 
  formatC(width = 2, flag = "0") %>%
  {str_c("./data/ave_vector/area-", ., ".csv")} %>% 
  map(~ read_csv(.x, col_types = "dddd"))

num_vectors <- 1:20 %>% 
  formatC(width = 2, flag = "0") %>%
  {str_c("./data/ave_vector/num-", ., ".csv")} %>% 
  map(~ read_csv(.x, col_types = "dddd"))

test_data <- read_csv(args,
                      col_names = c("time", "x", "y", "z"),
                      col_types = "dddd") %>% slice(1:nrow(area_vectors[[1]]))

now_time <- modify(test_data["time"], ~ .x - test_data[1, "time"] %>% flatten_dbl)
fix_test_data <- tibble("now_time" = now_time %>% flatten_dbl(),
                        "x" = test_data[2] %>% flatten_dbl(),
                        "y" = test_data[3] %>% flatten_dbl(),
                        "z" = test_data[4] %>% flatten_dbl())

get_similarity <- function(vector1, vector2) {
  x <- dtw(vector1$x, vector2$x, step=asymmetricP1, keep=TRUE)$distance
  y <- dtw(vector1$y, vector2$y, step=asymmetricP1, keep=TRUE)$distance
  z <- dtw(vector1$z, vector2$z, step=asymmetricP1, keep=TRUE)$distance
  sum(x, y, z)
}

area_similarity <- 1:length(area_vectors) %>%
  map_dbl(~ get_similarity(area_vectors[[.x]], fix_test_data))

area <- area_similarity %>% {which(min(.) == .)}

num_similarity <- 1:length(num_vectors) %>%
  map_dbl(~ get_similarity(num_vectors[[.x]], fix_test_data))

num <- num_similarity %>% {which(min(.) == .)}

area_names <- c("bull", "in single", "triple ring", "out single", "double ring", "out board")

if(area == 1 || area == 6){
  result <- area_names[area]
}else{
  result <- str_c(area_names[area], "(", num, ")")
}

print(result)

# gg <- acc002 %>%
#   ggplot(aes(x = now_time, y = y)) +
#   geom_line() +
#   theme_gray(base_size = 20, base_family = "Helvetica")
# 
# print(gg)