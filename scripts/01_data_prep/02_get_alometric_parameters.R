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

# from MPA tools
abnt <- MPAtools::abnt %>% 
  select(species = GeneroEspecie, a_mpa = a, b_mpa = b) %>% 
  distinct()

# From fishbase
spp_list <- clean_fish %>% 
  pull(species) %>% 
  unique() %>% 
  sort()

alo_spp <- length_weight(species_list = spp_list) %>% 
  clean_names() %>% 
  select(species, a, b) %>% 
  group_by(species) %>% 
  summarize(a = mean(a, na.rm = T),
            b = mean(b, na.rm = T),
            n = n()) %>% 
  left_join(abnt, by = "species") %>% 
  mutate(a = coalesce(a, a_mpa),
         b = coalesce(b, b_mpa)) %>% 
  select(species, a, b, n)

alo_genus <- alo_spp %>% 
  mutate(genus = str_extract(species, "[:alpha:]+")) %>% 
  group_by(genus) %>% 
  summarize(a_g = weighted.mean(a, na.rm = T, w = n),
         b_g = weighted.mean(b, na.rm = T, w = n)) %>% 
  ungroup()

length_weight <- clean_fish %>% 
  select(genus, species) %>% 
  distinct() %>% 
  left_join(alo_spp, by = "species") %>% 
  left_join(alo_genus, by = "genus") %>% 
  mutate(a = coalesce(a, a_g),
         b = coalesce(b, b_g)) %>% 
  select(species, a, b)

# Save data
write_csv(x = length_weight,
          file = here("data", "processed_data", "length_weight_parameters.csv"))
