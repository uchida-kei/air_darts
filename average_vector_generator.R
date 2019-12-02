invisible({rm(list=ls());gc();gc()})

library(ggplot2)
ggplot() + theme_set(theme_bw(base_size = 14,base_family="HiraKakuProN-W3"))

library(readr)
library(tibble)
library(purrr)
library(stringr)

# fileを読み込み列"time"を"now_time"に置き換える
read_acc_table <- function(file_name) {
  data <- read_csv(str_c("./data/acc/", file_name),
                   col_names = c("time", "x", "y", "z"),
                   col_types = "dddd")
  
  # hask_loggerでは端末起動時からの時間を記録するため最初の行を原点にする
  now_time <- modify(data["time"], ~ .x - data[1, "time"] %>% flatten_dbl)
  
  tibble("now_time" = now_time %>% flatten_dbl(),
         "x" = data[2] %>% flatten_dbl(),
         "y" = data[3] %>% flatten_dbl(),
         "z" = data[4] %>% flatten_dbl())
}

# 同カテゴリのベクトル群から平均ベクトルを算出，保存
write_ave_vector <- function(target_num, id, type) {
  # ベクトルが存在しないカテゴリは処理しない
  if(target_num %>% length()) {
    # ベクトルの合計をベクトルの数で除算
    reduce(slice_acc[target_num], `+`) %>% `/`(length(target_num)) %>% 
      write_csv(path = str_c("./data/ave_vector/",
                             type,
                             "-",
                             formatC(id, width = 2, flag = "0"),
                             ".csv"))
  }
  invisible()
}

label_data <- read_csv("./data/air_darts_label.csv",
                       col_types = "iiiii")
area_list <- 1:6 %>% map(~ which(label_data[2] == .x))
num_list <- 1:20 %>% map(~ which(label_data[3] == .x))

name_list <- 1:60 %>% formatC(width = 4, flag = "0") %>% str_c("-acc.csv")
acc <- name_list %>% map(read_acc_table)
min_len <- acc %>% map_int(nrow) %>% min()
slice_acc <- acc %>% map(~ .x %>% slice(1:min_len))

map(1:length(area_list), ~ write_ave_vector(area_list[[.x]], .x, "area"))
map(1:length(num_list), ~ write_ave_vector(num_list[[.x]], .x, "num"))

