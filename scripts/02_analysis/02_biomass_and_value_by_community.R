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
  cowplot,
  ggrepel,
  ggridges,
  tidyverse 
)

# Load data --------------------------------------------------------------------
# Fish biomass
fish_biomass <-
  read_csv(here("data", "processed_data", "fish_biomass_by_species.csv"))

# Invert biomass
invert_biomass <-
  read_csv(here( "data", "processed_data", "invertebrate_biomass_by_species.csv"))

# Prices
prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv"))

## PROCESSING ##################################################################

# Value of biomass -------------------------------------------------------------
# For fish
fish_biomass_value <- fish_biomass %>%
  left_join(prices, by = c("family", "group")) %>%
  mutate(value_MXN_hect = mean_price * biomass_kg_hect)

# For inverts
invert_biomass_value <- invert_biomass %>%
  left_join(prices, by = c("family", "group")) %>%
  mutate(value_MXN_hect = mean_price * biomass_kg_hect)

# Combine all data sets
biomass_value <- rbind(fish_biomass_value, invert_biomass_value) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate"),
         zone = str_replace(zone, "Reserva", "Reserve")) %>% 
  mutate(community = fct_relevel(community, com_order))

# Now summarize total value by group and by transect ---------------------------
biomass_by_transect <- biomass_value %>%
  drop_na(mean_price) %>%
  group_by(year, community, site, zone, group, transect) %>%
  summarize(
    biomass_kg_hect = sum(biomass_kg_hect, na.rm = T),
    value_MXN_hect = sum(value_MXN_hect, na.rm = T) / 1000) %>% 
  ungroup()

## EXPORT ######################################################################
# Export the main panel --------------------------------------------------------
saveRDS(obj = biomass_value,
        file = here("data", "output_data", "biomass_value_panel.rds"))

saveRDS(obj = biomass_by_transect,
        file = here("data", "output_data", "summarized_biomass_value_by_transect.rds"))
