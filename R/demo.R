## ---- load-pkg
library(tidyverse)
library(tsibble)

## ---- load-data
tb_small <- read_rds("data/tb_small.rds")
tb_small %>% 
  knitr::kable(booktabs = TRUE, caption = "(ref:tb-sub)", linesep = "") %>% 
  kableExtra::kable_styling(position = "center")
