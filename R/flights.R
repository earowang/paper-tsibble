## ---- load-flights-pkgs
library(lubridate)
library(tidyverse)
library(tsibble)
library(forcats)

## ---- load-flights
flights <- read_rds("data/flights.rds")

## ---- map-airlines
# devtools::install_github("heike/ggmapr")
library(ggmapr)
origin_dest <- flights %>% 
  distinct(origin, origin_state, dest, dest_state)
airports <- read_rds("data/airports.rds")
map_dat <- origin_dest %>% 
  left_join(airports, by = c("origin" = "faa")) %>% 
  rename(long = lon) %>% 
  shift(origin_state == "HI", shift_by = c(52.5, 5.5)) %>%
  scale(origin_state == "AK", scale = 0.3, set_to = c(-117, 27)) %>%
  rename(origin_lat = lat, origin_lon = long)  %>% 
  left_join(select(airports, faa, lon, lat), by = c("dest" = "faa")) %>% 
  rename(long = lon) %>% 
  shift(dest_state == "HI", shift_by = c(52.5, 5.5)) %>%
  scale(dest_state == "AK", scale = 0.3, set_to = c(-117, 27)) %>%
  rename(dest_lat = lat, dest_lon = long)

states <- states %>%
  shift(NAME == "Hawaii", shift_by = c(52.5, 5.5)) %>%
  scale(NAME == "Alaska", scale = 0.3, set_to = c(-117, 27)) %>%
  filter(lat > 20)

ggplot() +
  geom_polygon(data= states, aes(x = long, y = lat, group = group), 
    fill = "white", colour = "grey60") +
  geom_segment(data = map_dat, aes(
    x = origin_lon, y = origin_lat, xend = dest_lon, yend = dest_lat
  ), alpha = 0.1, size = 0.4, colour = "#762a83") +
  geom_point(data = map_dat, aes(x = origin_lon, y = origin_lat), 
    colour = "#a6dba0", size  = 0.7) +
  coord_map("albers", parameters = c(30, 45)) +
  ggthemes::theme_map()

## ---- glimpse
glimpse(flights)

## ---- print
print(flights, width = 80)

## ---- tsibble
us_flights <- flights %>% 
  as_tsibble(
    index = sched_dep_datetime, key = id(flight, origin), 
    regular = FALSE
  )

## ---- print-tsibble
us_flights

## ---- filter
us_flights %>% 
  filter(sched_dep_datetime < yearmonth("201703"))

## ---- select
us_flights %>% 
  select(flight, dep_delay)

## ---- summarise
us_flights %>% 
  summarise(avg_delay = mean(dep_delay))

## ---- index-by
us_flights %>% 
  index_by(
    dep_datehour = floor_date(sched_dep_datetime, unit = "hour")
  ) %>% 
  summarise(avg_delay = mean(dep_delay))

## ---- carrier-delayed
delayed_carrier <- us_flights %>% 
  mutate(delayed = dep_delay > 15) %>%
  group_by(carrier) %>% 
  index_by(year = year(sched_dep_datetime)) %>% 
  summarise(
    Ontime = sum(delayed == 0),
    Delayed = sum(delayed)
  ) %>% 
  mutate(carrier = reorder(carrier, - (Ontime + Delayed))) %>% 
  gather(delayed, n_flights, Ontime:Delayed)

library(ggmosaic)
ggplot(data = delayed_carrier) +
  geom_mosaic(aes(x = product(carrier), fill = delayed, weight = n_flights)) +
  scale_fill_brewer(palette = "Dark2", name = "Delayed") +
  theme(legend.position = "bottom") +
  xlab("Carrier") +
  ylab("Delayed")

## ----- n-flights
dep_delay_fct <- as_factor(c("ontime", "15-60 mins", "60+mins"))
n_flights <- us_flights %>% 
  mutate(
    dep_delay_break = case_when(
      dep_delay <= 15 ~ dep_delay_fct[1],
      dep_delay <= 60 ~ dep_delay_fct[2],
      TRUE ~ dep_delay_fct[3])
  ) %>% 
  group_by(dep_delay_break) %>% 
  index_by(dep_datehour = floor_date(sched_dep_datetime, unit = "hour")) %>% 
  summarise(n_flight = n()) %>% 
  mutate(
    hour = hour(dep_datehour), 
    wday = wday(dep_datehour, label = TRUE, week_start = 1),
    date = as_date(dep_datehour)
  )

## ---- delayed-facet
n_flights %>% 
  ggplot(aes(x = hour, y = n_flight, group = date)) +
  geom_line(alpha = 0.25, size = 0.4) +
  facet_grid(dep_delay_break ~ wday, scales = "free_y") +
  xlab("Time of day") +
  ylab("Number of flights") +
  scale_x_continuous(breaks = seq(6, 23, by = 6)) +
  theme_remark()

## ---- quantile
hr_qtl <- us_flights %>% 
  index_by(dep_datehour = floor_date(sched_dep_datetime, unit = "hour")) %>% 
  summarise(    
    qtl50 = quantile(dep_delay, 0.5),
    qtl80 = quantile(dep_delay, 0.8),
    qtl95 = quantile(dep_delay, 0.95)
  ) %>% 
  mutate(
    hour = hour(dep_datehour), 
    wday = wday(dep_datehour, label = TRUE, week_start = 1),
    date = as_date(dep_datehour)
  ) %>% 
  gather(key = qtl, value = dep_delay, qtl50:qtl95)

## ---- qtl1
us_flights %>% 
  index_by(dep_datehour = floor_date(sched_dep_datetime, unit = "hour")) %>% 
  summarise(    
    qtl50 = quantile(dep_delay, 0.5),
    qtl80 = quantile(dep_delay, 0.8),
    qtl95 = quantile(dep_delay, 0.95)
  )

## ---- qtl2
us_flights %>% 
  index_by(dep_datehour = floor_date(sched_dep_datetime, unit = "hour")) %>% 
  summarise(    
    qtl50 = quantile(dep_delay, 0.5),
    qtl80 = quantile(dep_delay, 0.8),
    qtl95 = quantile(dep_delay, 0.95)
  ) %>% 
  mutate(
    hour = hour(dep_datehour), 
    wday = wday(dep_datehour, label = TRUE, week_start = 1),
    date = as_date(dep_datehour)
  )

## ---- qtl3
us_flights %>% 
  index_by(dep_datehour = floor_date(sched_dep_datetime, unit = "hour")) %>% 
  summarise(    
    qtl50 = quantile(dep_delay, 0.5),
    qtl80 = quantile(dep_delay, 0.8),
    qtl95 = quantile(dep_delay, 0.95)
  ) %>% 
  mutate(
    hour = hour(dep_datehour), 
    wday = wday(dep_datehour, label = TRUE, week_start = 1),
    date = as_date(dep_datehour)
  ) %>% 
  gather(key = qtl, value = dep_delay, qtl50:qtl95)

## ---- draw-qtl
break_cols <- c(
  "qtl95" = "#d7301f", 
  "qtl80" = "#fc8d59", 
  "qtl50" = "#fdcc8a"
)

qtl_label <- c(
  "qtl50" = "50%",
  "qtl80" = "80%", 
  "qtl95" = "95%" 
)

min_y <- hr_qtl %>% 
  filter(hour(dep_datehour) > 4) %>% 
  pull(dep_delay) %>%
  min()

hr_qtl %>% 
  filter(hour(dep_datehour) > 4) %>% 
  ggplot(aes(x = hour, y = dep_delay, group = date, colour = qtl)) +
  geom_hline(yintercept = 15, colour = "#9ecae1", size = 2) +
  geom_line(alpha = 0.5) +
  facet_grid(
    qtl ~ wday, scales = "free_y", 
    labeller = labeller(qtl = as_labeller(qtl_label))
  ) +
  xlab("Time of day") +
  ylab("Depature delay") + 
  scale_x_continuous(limits = c(0, 23), breaks = seq(6, 23, by = 6)) +
  scale_colour_manual(values = break_cols, guide = FALSE) +
  expand_limits(y = min_y)

## ---- nyc_flights
nyc_flights <- us_flights %>% 
  filter(origin %in% c("JFK", "LGA", "EWR"))

nyc_delay <- nyc_flights %>% 
  mutate(delayed = dep_delay > 15) %>% 
  group_by(origin) %>% 
  index_by(sched_dep_date = floor_date(sched_dep_datetime, unit = "hour")) %>% 
  summarise(
    n_flights = n(),
    n_delayed = sum(delayed)
  ) %>% 
  mutate(pct_delay = n_delayed / n_flights)

nyc_delay <- fill_na(nyc_delay)

nyc_delay %>% 
  ggplot(aes(x = sched_dep_date, y = pct_delay, colour = origin)) +
  geom_line() +
  facet_grid(origin ~ .) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

## ----- nyc-weekly-ma
nyc_delay %>% 
  group_by(origin) %>% 
  mutate(ma_delay = slide_dbl(pct_delay, ~ mean(., na.rm = TRUE), .size = 24 * 7, .align = "center-left")) %>% 
  ggplot(aes(x = sched_dep_date)) +
  geom_line(aes(y = pct_delay), colour = "grey80", size = 0.8) +
  geom_line(aes(y = ma_delay, colour = origin), size = 1) +
  facet_grid(origin ~ .) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
  xlab("Date") +
  ylab("Departure delay")

## ----- nyc-monthly-ma
nyc_delay %>% 
  mutate(yrmth = yearmonth(sched_dep_date)) %>% 
  group_by(origin, yrmth) %>% 
  nest() %>% 
  group_by(origin) %>% 
  mutate(monthly_ma = slide_dbl(data, 
    ~ mean(.$pct_delay, na.rm = TRUE), .size = 2, .bind = TRUE
  )) %>% 
  unnest(key = id(origin)) %>% 
  ggplot() +
  geom_line(aes(x = sched_dep_date, y = pct_delay), colour = "grey80", size = 0.8) +
  geom_line(aes(x = as.POSIXct(yrmth), y = monthly_ma, colour = origin), size = 1) +
  facet_grid(origin ~ .) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
  xlab("Date") +
  ylab("Departure delay")

## ---- calendar
library(sugrrants)
nyc_cal <- nyc_delay %>% 
  mutate(
    hour = hour(sched_dep_date), 
    date = as_date(sched_dep_date)
  ) %>% 
  group_by(origin) %>% 
  frame_calendar(x = hour, y = pct_delay, date = date)

p_cal <- nyc_cal %>% 
  ggplot(aes(x = .hour, y = .pct_delay, group = date, colour = origin)) +
  geom_line() +
  facet_grid(~ origin) +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
prettify(p_cal, label.padding = unit(0.02, "lines"))
