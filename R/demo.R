## ---- load-pkg
library(tidyverse)
library(tsibble)

## ---- tb-sub
tb_small <- read_rds("data/tb-small.rds")
tb_small %>% 
  filter(year > 2010) %>% 
  knitr::kable(booktabs = TRUE, caption = "(ref:tb-sub)", linesep = "") %>% 
  kableExtra::kable_styling(position = "center")

## ---- tb-print
as_tsibble(tb_small, key = id(country, gender), index = year) %>% 
  filter(year > 2010) %>% 
  print(n = 5)

## ---- tb-au
tb_au <- tb_small %>% 
  filter(country == "Australia") %>% 
  group_by(year) %>% 
  summarise(count = sum(count))

## ---- slide-animate
library(gganimate)
slide_window <- slider(tb_au$year, .size = 5) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
ggplot() +
  geom_line(aes(year, count), data = tb_au) +
  geom_point(aes(year, count), data = tb_au) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = slide_window, fill = "#9ecae1", alpha = 0.6) +
  xlab("Year") +
  ylab("Count") +
  ylim(c(0, max(tb_au$count))) +
  theme_bw() +
  transition_manual(group)

## ---- tile-animate
tile_window <- tiler(tb_au$year, .size = 5) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
ggplot() +
  geom_line(aes(year, count), data = tb_au) +
  geom_point(aes(year, count), data = tb_au) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = tile_window, fill = "#9ecae1", alpha = 0.6) +
  xlab("Year") +
  ylab("Count") +
  ylim(c(0, max(tb_au$count))) +
  theme_bw() +
  transition_manual(group)

## ---- stretch-animate
stretch_window <- stretcher(tb_au$year, .init = 5) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
ggplot() +
  geom_line(aes(year, count), data = tb_au) +
  geom_point(aes(year, count), data = tb_au) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = stretch_window, fill = "#9ecae1", alpha = 0.6) +
  xlab("Year") +
  ylab("Count") +
  ylim(c(0, max(tb_au$count))) +
  theme_bw() +
  transition_manual(group)
