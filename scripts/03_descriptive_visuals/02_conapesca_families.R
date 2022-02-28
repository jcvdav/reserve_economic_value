######################################################
#title#
######################################################
# 
# Purpose
#
######################################################


library(here)
library(tidyverse)

prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv"))

my_fn <- function(x){
  # browser()
  paste(x$conapesca, collapse = ", ")
}

conapesca <- read_csv(here("data", "processed_data", "conapesca_families.csv")) %>% 
  left_join(prices, by = c("group", "family")) %>% 
  arrange(group, family) %>% 
  select(-median_price) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-sd_price) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate")) %>% 
  group_by(group, family, mean_price) %>% 
  nest() %>% 
  mutate(conapesca = map_chr(data, my_fn)) %>% 
  select(group, family, conapesca, mean_price)

write_csv(x = conapesca,
          file = here("results", "tab", "conapesca_families.csv"))



