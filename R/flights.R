## ---- load-pkgs
library(lubridate)
library(tidyverse)
library(tsibble)
library(forcats)

## ---- load-data
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
  ), alpha = 0.2, size = 0.5, colour = "#762a83") +
  geom_point(data = map_dat, aes(x = origin_lon, y = origin_lat), 
    colour = "#f1a340", size  = 1.5) +
  coord_map("albers", parameters = c(30, 45)) +
  ggthemes::theme_map()

## ---- glimpse
glimpse(flights)

## ---- print
print(flights, width = 80)

## ---- tsibble
us_flights <- flights %>% 
  as_tsibble(
    index = sched_dep_datetime,
    key = id(flight), 
    regular = FALSE
  )

## ---- print-tsibble
us_flights

## ---- filter
us_flights %>% 
  filter(month(sched_dep_datetime) == 10)

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

hr_qtl %>% 
  filter(hour(dep_datehour) > 4) %>% 
  ggplot(aes(x = hour, y = dep_delay, group = date, colour = qtl)) +
  geom_hline(yintercept = 0, colour = "#9ecae1", size = 2) +
  geom_line(alpha = 0.5) +
  facet_grid(
    qtl ~ wday, scales = "free_y", 
    labeller = labeller(qtl = as_labeller(qtl_label))
  ) +
  xlab("Time of day") +
  ylab("Depature delay") + 
  scale_x_continuous(limits = c(0, 23), breaks = seq(6, 23, by = 6)) +
  scale_colour_manual(values = break_cols, guide = FALSE) +
  theme_remark()

## ---- carrier
carrier_delay <- us_flights %>% 
  group_by(carrier) %>% 
  index_by(sched_date = as_date(sched_dep_datetime)) %>% 
  summarise(avg_delay = mean(dep_delay)) 
carrier_delay

## ---- carrier-plot
carrier_delay %>% 
  ggplot(aes(x = sched_date, y = avg_delay)) +
  geom_line(size = 0.8) +
  facet_grid(carrier ~ .) +
  xlab("Date") +
  ylab("Departure delay")

## ----- carrier-ma
carrier_delay_ma <- carrier_delay %>% 
  group_by(carrier) %>% 
  mutate(ma_delay = slide_dbl(avg_delay, mean, .size = 7, .align = "center"))
carrier_delay_ma

## ----- carrier-ma-plot
carrier_delay_ma %>% 
  ggplot(aes(x = sched_date)) +
  geom_line(aes(y = avg_delay), colour = "grey80", size = 0.8) +
  geom_line(aes(y = ma_delay), colour = "#3182bd", size = 1) +
  facet_grid(carrier ~ .) +
  xlab("Date") +
  ylab("Departure delay")
