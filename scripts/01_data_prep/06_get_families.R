################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  rfishbase,
  janitor,
  tidyverse
)

# Load data --------------------------------------------------------------------
clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv"))

## PROCESSING ##################################################################

# Get families -----------------------------------------------------------------
# List of species
spp_list <- clean_fish %>% 
  select(genus, species) %>% 
  distinct()

# Ger fishbase taxonomy
taxa_spp <- load_taxa() %>% 
  as_tibble() %>% 
  clean_names() %>% 
  drop_na(family) %>% 
  select(species, genus, family)

# Get different genuses
taxa_genus <- taxa_spp %>% 
  select(genus, family) %>% 
  distinct()

# Combine all data
spp_tbl <- spp_list %>% 
  left_join(taxa_spp, by = c("species", "genus")) %>% 
  left_join(taxa_genus, by = "genus") %>% 
  mutate(family = coalesce(family.x, family.y)) %>% 
  select(family, genus, species) %>% 
  distinct() %>% 
  mutate(family = ifelse(family == "Polyprionidae", "Serranidae", family))

## EXPORT ######################################################################
write_csv(x = spp_tbl,
          file = here("data", "processed_data", "family_names.csv"))
