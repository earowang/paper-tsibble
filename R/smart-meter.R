## ---- load-elec
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

## ---- elec-gaps
gap_df <- has_gaps(elec_ts)
# sum(gap_df$.gaps) / NROW(gap_df)

## ---- scan-gaps
elec_gaps <- scan_gaps(elec_ts)

## ---- count-gaps
customer_na <- elec_ts %>% 
  filter(customer_id %in% (gap_df %>% filter(.gaps) %>% pull(customer_id)))

count_na_df <- customer_na %>% 
  count_gaps()

count_na_df %>% 
  mutate(
    customer_id = as.factor(customer_id) %>% fct_lump(50) %>% fct_reorder(.n)
  ) %>% 
  ggplot(aes(x = customer_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  )

## ---- calendar-plot
library(sugrrants)
elec_cal <- elec_ts %>% 
  summarise(avg = mean(general_supply_kwh)) %>% 
  mutate(
    date = as_date(reading_datetime), 
    time = hms::as.hms(reading_datetime, tz = "UTC")
  ) %>% 
  frame_calendar(x = time, y = avg, date = date)

p_cal <- elec_cal %>% 
  ggplot(aes(.time, .avg, group = date)) +
  geom_line()
prettify(p_cal)

## ---- elec-quantiles
qtl_grid <- seq(0.1, 0.9, 0.002)
elec_qtl <- elec_ts %>% 
  summarise(
    value = list(quantile(general_supply_kwh, qtl_grid, na.rm = TRUE)),
    qtl = list(qtl_grid)
  ) %>% 
  unnest(key = id(qtl)) %>% 
  mutate(
    time = hms::as.hms(reading_datetime, tz = "UTC"),
    date = as_date(reading_datetime)
  )

elec_med <- elec_qtl %>% 
  filter(qtl == 0.5)

elec_qtl %>% 
  ggplot(aes(x = time, y = value, colour = qtl, group = qtl)) +
  geom_line() +
  geom_line(data = elec_med, colour = "#de2d26", size = 0.5) +
  sugrrants::facet_calendar(~ date, ncol = 3, format = "%m/%d") +
  scale_x_time(
    breaks = hms::as.hms(c("6:00:00", "18:00:00")),
    labels = c("6:00", "18:00")
  ) +
  scale_colour_viridis_c() +
  theme(legend.position = "none")

## ---- fill-gaps
# make complete series
elec_ts <- elec_ts %>% 
  fill_gaps()

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

## ---- forecast
library(fable)
elec_mth <- elec_ts %>% 
  filter_index(~ "2013-01-30") %>% 
  index_by(datehour = floor_date(reading_datetime, "hour")) %>% 
  summarise(avg_supply = mean(general_supply_kwh)) %>% 
  mutate(
    hour = hour(datehour),
    date = as_date(datehour)
  )

elec_fc <- elec_mth %>% 
  model(ets = ETS(avg_supply)) %>% 
  forecast(h = 24)
elec_fc$date <- as_date(elec_fc$datehour)

# elec_fc %>% 
#   autoplot(data = elec_mth) +
#   sugrrants::facet_calendar(~ date)

elec_mth %>% 
  ggplot(aes(x = datehour, y = avg_supply)) +
  geom_line() +
  geom_forecast(
    aes(ymin = lower, ymax = upper, level = level),
    fortify(elec_fc) %>% mutate(date = as_date(datehour)), stat = "identity"
  ) +
  NULL
  sugrrants::facet_calendar(~ date)
