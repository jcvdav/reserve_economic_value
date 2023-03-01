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
  tidyverse
)

# Load data --------------------------------------------------------------------
# Transect data
clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv")) 
clean_inverts <- read_csv(file = here("data", "processed_data", "clean_invertebrate_transects.csv")) 

# Length-weight and weight factors
lw <- read_csv(file = here("data", "processed_data", "length_weight_parameters.csv"))
dens_weight_inverts <- read.csv(here("data", "raw_data", "invertebrate_weight_factors.csv"))

family_prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv")) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate"))

families <- unique(family_prices$family)

family <-
  read_csv(file = here("data", "processed_data", "family_names.csv")) %>% 
  select(family, species)

## PROCESSING ##################################################################

# Fish biomass -----------------------------------------------------------------
biomass <- clean_fish %>% 
  left_join(family, by = "species") %>% 
  filter(family %in% families) %>%
  left_join(lw, by = "species") %>% 
  filter(size >= 21) %>% 
  mutate(w = (a * (size ^ b)),
         biomass = abundance * w) %>% 
  group_by(year, community, site, zone, transect, group, family) %>% 
  summarize(biomass_g_m2 = sum(biomass, na.rm = T) / 60,
            biomass_kg_m2 = biomass_g_m2 / 1000,
            biomass_kg_hect = biomass_kg_m2 * 10000) %>% 
  ungroup() %>% 
  select(-c(biomass_g_m2, biomass_kg_m2))


# Invertebrate biomass ---------------------------------------------------------
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

## EXPORT ######################################################################
# Fish biomass -----------------------------------------------------------------
write_csv(x = biomass,
          file = here("data", "processed_data", "fish_biomass_by_species.csv"))
# Invert biomass ---------------------------------------------------------------
write_csv(x = invert_dens,
          file = here("data", "processed_data", "invertebrate_biomass_by_species.csv"))
  
  
  
