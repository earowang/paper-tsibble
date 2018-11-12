library(tidyverse)
library(lubridate)
library(tsibble)

customer <- read_rds("data/customer.rds")
elec <- read_rds("data/smart-meter13.rds")

elec_ts <- elec %>% 
  build_tsibble(
    key = id(customer_id), index = reading_datetime,
    validate = FALSE, ordered = TRUE
  )

gap_df <- has_gaps(elec_ts)

gap_df %>% 
  ggplot(aes(x = factor(1), fill = .gaps)) +
  geom_bar(position = "fill") +
  coord_flip()

customer_na <- elec_ts %>% 
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

elec_full <- elec_ts %>% 
  fill_na()

elec_na <- elec_full %>% 
  filter(is.na(general_supply_kwh))

elec_na_mth <- elec_na %>% 
  mutate(
    mth = month(reading_datetime, label = TRUE),
    is_na = 1L
  ) %>% 
  group_by(customer_id) %>% 
  index_by(mth) %>% 
  summarise(n_na = sum(is_na))

elec_na_mth %>% 
  ggplot(aes(x = mth)) +
  geom_bar()

elec_gen <- elec_full %>% 
  left_join(
    customer %>% select(customer_key, has_gas, has_aircon),
    by = c("customer_id" = "customer_key")
  )

elec_gen %>% 
  as_tibble() %>% 
  group_by(has_gas, has_aircon) %>% 
  summarise(avg_kwh = mean(general_supply_kwh, na.rm = TRUE))

names(customer)
