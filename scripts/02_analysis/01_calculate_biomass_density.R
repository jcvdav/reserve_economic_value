######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(rfishbase)
library(janitor)
library(MPAtools)
library(tidyverse)

# Load data
clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv"))
lw <- read_csv(file = here("data", "processed_data", "length_weight_parameters.csv"))
family <- read_csv(file = here("data", "processed_data", "family_names.csv"))
prices <- read_csv(file = here("data", "processed_data", "family_prices.csv"))

biomass <- clean_fish %>% 
  filter(year == 2019) %>% 
  left_join(lw, by = "species") %>% 
  left_join(family, by = c("species", "genus")) %>% 
  mutate(w = a * (size ^ b),
         b = abundance * w) %>% 
  group_by(community, site, transect, family) %>% 
  summarize(b = sum(b, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(community, site, family) %>% 
  summarize(b = mean(b, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(prices, by = "family") %>% 
  replace_na(replace = list("price" = 0)) %>% 
  mutate(economic = ifelse(price > 0, "Fisheries value", "No fisheries value"))

ggplot(biomass, aes(x = community, y = b, fill = economic)) + 
  geom_col() + 
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend("Value")) +
  theme_bw()
