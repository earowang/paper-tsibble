## ---- load-pkg
library(tidyverse)
library(tsibble)

## ---- tb-sub
tb_small <- read_rds("data/tb-small.rds")
tb_small %>% 
  knitr::kable(booktabs = TRUE, caption = "(ref:tb-sub)", linesep = "") %>% 
  kableExtra::kable_styling(position = "center")

## ---- tb-print
as_tsibble(tb_small, key = id(country, gender), index = year) %>% 
  print(n = 5)
