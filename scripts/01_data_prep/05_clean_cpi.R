######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(janitor)
library(tidyverse)

cpi_all <- read_csv(here("data", "raw_data", "CPI_all.csv"), show_col_types = F) %>% 
  clean_names()

cpi_mex <- cpi_all %>% 
  filter(location == "MEX",
         indicator == "CPI",
         subject == "FOOD",
         measure == "AGRWTH",
         frequency == "A") %>% 
  select(year = time, cpi = value) %>% 
  mutate(fact = cpi / cpi[year == 2019])

write_csv(cpi_mex, here("data", "processed_data", "mex_cpi.csv"))
