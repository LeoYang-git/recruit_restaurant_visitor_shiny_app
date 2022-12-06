## Set up
library(tidyverse)
library(shiny)
library(lubridate)
library(plotly)
library(leaflet)

## Load Data 
raw_air_reserve <- read.csv("data/air_reserve.csv")
raw_air_store_info <- read.csv("data/air_store_info.csv")
raw_air_visit_data <- read.csv("data/air_visit_data.csv")
raw_date_info <- read.csv("data/date_info.csv")
raw_hpg_reserve <- read.csv("data/hpg_reserve.csv")
raw_hpg_store_info <- read.csv("data/hpg_store_info.csv")
raw_store_id_relation <- read.csv("data/store_id_relation.csv")

# 
# # ## Derived Data 
# df_rest_dual_list <- raw_store_id_relation %>% 
#   left_join(raw_air_store_info, by = "air_store_id") %>% 
#   left_join(raw_hpg_store_info, by = "hpg_store_id")
# 
df_res_air <-  raw_air_reserve %>%
  left_join(raw_air_store_info) %>% 
  mutate(visit_d = date(visit_datetime),
         visit_h = hour(visit_datetime)) %>% 
  left_join(raw_date_info %>% mutate(calendar_date = date(calendar_date)),
            by = c("visit_d" = "calendar_date")) %>% 
  rename(cuisine = air_genre_name)

df_res_hpg <- raw_hpg_reserve %>% 
  left_join(raw_hpg_store_info) %>% 
  mutate(visit_d = date(visit_datetime),
         visit_h = hour(visit_datetime)) %>% 
  left_join(raw_date_info %>% mutate(calendar_date = date(calendar_date)),
            by = c("visit_d" = "calendar_date")) %>% 
  rename(cuisine = hpg_genre_name)

df_vis_air <- raw_air_visit_data %>%
  left_join(raw_air_store_info) %>% 
  mutate(visit_d = date(visit_date)) %>% 
  left_join(raw_date_info %>% mutate(calendar_date = date(calendar_date)),
            by = c("visit_d" = "calendar_date")) %>% 
  rename(cuisine = air_genre_name)