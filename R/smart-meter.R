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

# ToDo: use tables to present implicit missingness proportions

customer_na <- elec_ts %>% 
  filter(customer_id %in% (gap_df %>% filter(.gaps) %>% pull(customer_id)))

count_na_df <- customer_na %>% 
  count_gaps()

# ToDo: do a hierarchical clustering for classifying customers

count_na_10 <- count_na_df %>% 
  count(customer_id) %>% 
  top_n(100) %>% 
  pull(customer_id)

count_na_df %>% 
  filter(customer_id %in% count_na_10) %>% 
  ggplot(aes(x = as.factor(customer_id))) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme(legend.position = "bottom")

elec_ts <- elec_ts %>% 
  fill_na()

elec_na <- elec_ts %>% 
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

elec_ts <- elec_ts %>% 
  mutate(season = if_else(
    time_in(reading_datetime, ~ "2013-03", "2013-10" ~ .),
    "Autumn-Winter", "Spring-Summer"
  ))

qtl_grid <- seq(0.1, 0.9, 0.01)
gas_aircon <- elec_ts %>% 
  inner_join(
    customer %>% 
      select(customer_key, has_gas, has_aircon) %>% 
      drop_na(has_gas, has_aircon),
    by = c("customer_id" = "customer_key")
  ) %>% 
  index_by(hms = hms::as.hms(reading_datetime)) %>% 
  group_by(season, has_gas, has_aircon) %>% 
  summarise(
    value = list(quantile(general_supply_kwh, qtl_grid, na.rm = TRUE)),
    qtl = list(qtl_grid)
  ) %>% 
  unnest(key = id(qtl))

gas_aircon %>% 
  ggplot(aes(x = hms, y = value, colour = qtl, group = qtl)) +
  geom_line() +
  facet_grid(has_aircon ~ has_gas + season, labeller = "label_both") +
  scale_colour_viridis_c()

gas_aircon_avg <- elec_ts %>% 
  inner_join(
    customer %>% 
      select(customer_key, has_gas, has_aircon) %>% 
      drop_na(has_gas, has_aircon),
    by = c("customer_id" = "customer_key")
  ) %>% 
  group_by(has_gas, has_aircon) %>% 
  summarise(avg_kwh = mean(general_supply_kwh, na.rm = TRUE)) %>% 
  ungroup()

gas_aircon_avg %>% 
  ggplot(aes(x = reading_datetime, y = avg_kwh, colour = has_gas)) +
  geom_point(size = 0.1) +
  facet_grid(has_aircon ~ has_gas, labeller = "label_both")

gas_aircon_avg %>% 
  ggplot(aes(x = reading_datetime, y = avg_kwh, colour = season)) +
  geom_point(size = 0.1) +
  facet_grid(has_aircon ~ has_gas, labeller = "label_both")

gas_aircon_avg %>% 
  ggplot(aes(x = season, y = avg_kwh, colour = has_gas)) +
  geom_boxplot() +
  facet_grid(has_aircon ~ ., labeller = "label_both") +
  scale_y_log10()

gas_aircon_avg %>% 
  ggplot(aes(x = season, y = avg_kwh, colour = has_aircon)) +
  geom_boxplot() +
  facet_grid(has_gas ~ ., labeller = "label_both") +
  scale_y_log10()
names(customer)
