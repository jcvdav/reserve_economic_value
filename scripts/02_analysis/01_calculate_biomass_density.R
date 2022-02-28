######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(tidyverse)

# Load data
clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv")) 
clean_inverts <- read_csv(file = here("data", "processed_data", "clean_invertebrate_transects.csv")) 

lw <- read_csv(file = here("data", "processed_data", "length_weight_parameters.csv"))
dens_weight_inverts <- read.csv(here("data", "raw_data", "invertebrate_weight_factors.csv")) %>% 
  select(-source)

family_prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv")) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate"))

families <- unique(family_prices$family)

family <-
  read_csv(file = here("data", "processed_data", "family_names.csv")) %>% 
  select(family, species)

# PROCESSING ###################################################################
# Fish
biomass <- clean_fish %>% 
  left_join(family, by = "species") %>% 
  filter(family %in% families) %>%
  left_join(lw, by = "species") %>% 
  mutate(large = 1 * (size >= 21),
         w = (a * (size ^ b)) * large,
         biomass = abundance * w) %>% 
  group_by(year, community, site, zone, transect, group, family) %>% 
  summarize(biomass_g_m2 = sum(biomass, na.rm = T) / 60,
            biomass_kg_m2 = biomass_g_m2 / 1000,
            biomass_kg_hect = biomass_kg_m2 * 10000) %>% 
  ungroup() %>% 
  select(-c(biomass_g_m2, biomass_kg_m2))

# Inverts
invert_dens <- clean_inverts %>% 
  filter(family %in% families) %>%
  left_join(dens_weight_inverts, by = c("species" = "taxonomic_group")) %>% 
  left_join(dens_weight_inverts, by = c("family" = "taxonomic_group")) %>% 
  mutate(w_fact =  coalesce(w_fact.x, w_fact.y)) %>% 
  select(- c(w_fact.x, w_fact.y)) %>% 
  drop_na(w_fact) %>%
  mutate(biomass = abundance * w_fact) %>% 
  group_by(year, community, site, zone, transect, group, family) %>% 
  summarize(biomass_g_m2 = sum(biomass, na.rm = T) / 60,
            biomass_kg_m2 = biomass_g_m2 / 1000,
            biomass_kg_hect = biomass_kg_m2 * 10000) %>% 
  ungroup() %>% 
  select(-c(biomass_g_m2, biomass_kg_m2))


write_csv(x = biomass,
          file = here("data", "processed_data", "fish_biomass_by_species.csv"))

write_csv(x = invert_dens,
          file = here("data", "processed_data", "invertebrate_biomass_by_species.csv"))
  
  
  
