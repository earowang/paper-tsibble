library(tidyverse)
library(lubridate)
library(tsibble)

tb <- read_csv("data/tb-2018-06-05.csv")

tb_small <- tb %>% 
  select(-region, -age) %>% 
  filter(
    country %in% c("Australia", "New Zealand", "United States of America"),
    year > 2010
  ) %>% 
  group_by(country, continent, gender, year) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()

write_rds(tb_small, "data/tb_small.rds")
