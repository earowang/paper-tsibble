library(tidyverse)
library(lubridate)
library(tsibble)

customer <- read_rds("data/customer_newcastle.rds")
eudm <- read_rds("data/eudm13_newcastle.rds")

eudm_ts <- eudm %>% 
  build_tsibble(
    key = id(customer_id), index = reading_datetime,
    validate = FALSE, ordered = TRUE
  )

gap_df <- has_gaps(eudm_ts)

gap_df %>% 
  ggplot(aes(x = factor(1), fill = .gaps)) +
  geom_bar(position = "fill")

customer_na <- eudm_ts %>% 
  filter(customer_id %in% (gap_df %>% filter(.gaps) %>% pull(customer_id)))

count_na_df <- customer_na %>% 
  count_gaps()

count_na_10 <- count_na_df %>% 
  count(customer_id) %>% 
  top_n(10) %>% 
  pull(customer_id)

count_na_df %>% 
  filter(customer_id %in% count_na_10) %>% 
  ggplot(aes(x = as.factor(customer_id))) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme(legend.position = "bottom")

eudm_full <- eudm_ts %>% 
  fill_na()

eudm_na <- eudm_full %>% 
  filter(is.na(general_supply_kwh))
