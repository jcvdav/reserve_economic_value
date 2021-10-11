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
            b = mean(b, na.rm = T)) %>% 
  left_join(abnt, by = "species") %>% 
  mutate(a = coalesce(a, a_mpa),
         b = coalesce(b, b_mpa)) %>% 
  select(species, a, b)

alo_bayes <- estimate(species_list = spp_list) %>% 
  clean_names() %>% 
  select(species, a_bayes = a, b_bayes = b)

alo_spp_bayes <- alo_spp %>% 
  left_join(alo_bayes, by = c("species")) %>% 
  mutate(a = coalesce(a, a_bayes),
         b = coalesce(b, b_bayes)) %>% 
  select(species, a, b)

alo_genus <- alo_spp_bayes %>% 
  mutate(genus = str_extract(species, "[:alpha:]+")) %>% 
  group_by(genus) %>% 
  summarize(a_g = mean(a, na.rm = T),
            b_g = mean(b, na.rm = T)) %>% 
  ungroup()

length_weight <- clean_fish %>% 
  select(genus, species) %>% 
  distinct() %>% 
  left_join(alo_spp_bayes, by = "species") %>% 
  left_join(alo_genus, by = "genus") %>% 
  mutate(a = coalesce(a, a_g),
         b = coalesce(b, b_g)) %>% 
  select(species, a, b)

# Save data
write_csv(x = length_weight,
          file = here("data", "processed_data", "length_weight_parameters.csv"))
