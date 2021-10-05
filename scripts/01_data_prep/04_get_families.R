######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

# Load libraries
library(here)
library(rfishbase)
library(janitor)
library(tidyverse)

clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv"))

# From fishbase
spp_list <- clean_fish %>% 
  select(genus, species) %>% 
  distinct()

taxa_spp <- load_taxa() %>% 
  as_tibble() %>% 
  clean_names() %>% 
  drop_na(family) %>% 
  select(species, genus, family)

taxa_genus <- taxa_spp %>% 
  select(genus, family) %>% 
  distinct()

spp_tbl <- spp_list %>% 
  left_join(taxa_spp, by = c("species", "genus")) %>% 
  left_join(taxa_genus, by = "genus") %>% 
  mutate(family = coalesce(family.x, family.y)) %>% 
  select(family, genus, species) %>% 
  distinct()

write_csv(x = spp_tbl,
          file = here("data", "processed_data", "family_names.csv"))
