######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(cowplot)
library(tidyverse)

# Load data
clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv")) 
lw <- read_csv(file = here("data", "processed_data", "length_weight_parameters.csv"))

biomass <- clean_fish %>% 
  left_join(lw, by = "species") %>% 
  mutate(w = a * (size ^ b),
         biomass = abundance * w) %>% 
  group_by(year, community, site, zone, transect, group, genus, species) %>% 
  summarize(biomass_g_m2 = sum(biomass, na.rm = T) / 60,
            biomass_kg_m2 = biomass_g_m2 / 1000,
            biomass_kg_hect = biomass_kg_m2 * 10000) %>% 
  ungroup() %>% 
  select(-c(biomass_g_m2, biomass_kg_m2))

write_csv(x = biomass,
          file = here("data", "processed_data", "biomass_by_species.csv"))
  
  
  
