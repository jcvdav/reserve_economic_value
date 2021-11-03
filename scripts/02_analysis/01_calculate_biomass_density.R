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
clean_inverts <- read_csv(file = here("data", "processed_data", "clean_invertebrate_transects.csv")) 
lw <- read_csv(file = here("data", "processed_data", "length_weight_parameters.csv"))
dens_weight_inverts <- read.csv(here("data", "raw_data", "invertebrate_weight_factors.csv")) %>% 
  select(-source)

biomass <- clean_fish %>% 
  # filter(year == 2019) %>% 
  left_join(lw, by = "species") %>% 
  mutate(w = a * (size ^ b),
         biomass = abundance * w) %>% 
  group_by(year, community, site, zone, transect, group, genus, species) %>% 
  summarize(biomass_g_m2 = sum(biomass, na.rm = T) / 60,
            biomass_kg_m2 = biomass_g_m2 / 1000,
            biomass_kg_hect = biomass_kg_m2 * 10000) %>% 
  ungroup() %>% 
  select(-c(biomass_g_m2, biomass_kg_m2)) 

invert_dens <- clean_inverts %>% 
  # filter(year == 2019) %>% 
  left_join(dens_weight_inverts, by = "family") %>% 
  mutate(biomass = abundance * w_fact) %>% 
  group_by(year, community, site, zone, transect, group, genus, family) %>% 
  summarize(biomass_g_m2 = sum(biomass, na.rm = T) / 60,
            biomass_kg_m2 = biomass_g_m2 / 1000,
            biomass_kg_hect = biomass_kg_m2 * 10000) %>% 
  ungroup() %>% 
  select(-c(biomass_g_m2, biomass_kg_m2, genus)) 


write_csv(x = biomass,
          file = here("data", "processed_data", "fish_biomass_by_species.csv"))

write_csv(x = invert_dens,
          file = here("data", "processed_data", "invertebrate_biomass_by_species.csv"))
  
  
  
