######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(tidyverse)

biomass <- read_csv(here("data", "processed_data", "biomass_by_species.csv"))
family <- read_csv(file = here("data", "processed_data", "family_names.csv"))
prices <- read_csv(file = here("data", "processed_data", "family_prices.csv"))

biomass_value <- biomass %>% 
  filter(zone == "Reserva") %>% 
  filter(biomass_kg_hect > 0) %>% 
  left_join(family, by = c("species", "genus")) %>% 
  left_join(prices, by = c("family", "group")) %>% 
  replace_na(replace = list("mean_price" = 0,
                            "median_price" = 0)) %>% 
  mutate(economic = ifelse(mean_price > 0, "Fisheries value", "No fisheries value"),
         value_mxp_hect = mean_price * biomass_kg_hect) 
  
biomass_by_community <- biomass_value %>% 
  group_by(year, community, site, zone, economic, group, transect) %>% 
  summarize(biomass_kg_hect = sum(biomass_kg_hect, na.rm = T),
            value_mxp_hect = sum(value_mxp_hect, na.rm = T)) %>% 
  group_by(community, group, economic) %>% 
  summarize(biomass_kg_hect_sd = sd(biomass_kg_hect, na.rm = T),
            biomass_kg_hect = median(biomass_kg_hect, na.rm = T),
            value_mxp_hect_sd = sd(value_mxp_hect, na.rm = T),
            value_mxp_hect = median(value_mxp_hect)) %>% 
  ungroup() %>% 
  mutate(community = fct_reorder(community, value_mxp_hect)) 

biomass_by_community %>% 
  filter(value_mxp_hect > 0) %>% 
  ggplot(aes(x = community, y = value_mxp_hect)) +
  geom_errorbar(aes(ymin = value_mxp_hect, ymax = value_mxp_hect + value_mxp_hect_sd), width = 0.25) +
  geom_col(fill = "steelblue", color = "black") +
  theme_bw() +
  coord_flip() +
  labs(x = "Comunidad", y = "Valor (MXP / hect)")
  

ggplot(biomass_by_community, aes(x = community, y = biomass_kg_hect, fill = economic)) +
  geom_point(#range(aes(ymin = biomass_kg_hect - biomass_kg_hect_sd, ymax = biomass_kg_hect + biomass_kg_hect_sd),
                  position = position_dodge(0.5),
                  shape = 21, size = 1) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()
