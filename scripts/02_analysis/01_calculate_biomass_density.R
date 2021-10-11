######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(rfishbase)
library(MPAtools)
library(cowplot)
library(tidyverse)

# Load data
clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv")) %>% 
  filter(zone == "Reserva")
lw <- read_csv(file = here("data", "processed_data", "length_weight_parameters.csv"))
family <- read_csv(file = here("data", "processed_data", "family_names.csv"))
prices <- read_csv(file = here("data", "processed_data", "family_prices.csv"))

biomass <- clean_fish %>% 
  filter(year == 2019) %>% 
  left_join(lw, by = "species") %>% 
  left_join(family, by = c("species", "genus")) %>% 
  mutate(w = a * (size ^ b),
         biomass = abundance * w) %>% 
  left_join(prices, by = "family") %>% 
  replace_na(replace = list("price" = 0)) %>% 
  mutate(economic = ifelse(price > 0, "Fisheries value", "No fisheries value")) %>% 
  group_by(community, site, transect, economic) %>% 
  summarize(biomass_g_m2 = sum(biomass, na.rm = T) / 60,
            biomass_kg_m2 = biomass_g_m2 / 1000,
            biomass_kg_hect = biomass_kg_m2 * 10000,
            value_mxp_hect = biomass_kg_hect * price) %>% 
  ungroup() %>% 
  group_by(community, economic) %>% 
  summarize(biomass_kg_hect_sd = sd(biomass_kg_hect, na.rm = T),
            biomass_kg_hect = mean(biomass_kg_hect, na.rm = T),
            value_mxp_hect_sd = sd(value_mxp_hect, na.rm = T),
            value_mxp_hect = mean(value_mxp_hect)) %>% 
  group_by(community) %>% 
  mutate(tot_biomass = sum(biomass_kg_hect)) %>% 
  ungroup() %>% 
  mutate(community = fct_reorder(community, tot_biomass))

p1 <- ggplot(biomass, aes(x = community, y = biomass_kg_hect / tot_biomass, fill = economic)) + 
  geom_col(color = "black") + 
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend("Value")) +
  theme_bw() +
  theme(legend.position = "None") +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  labs(x = "Community", y = "Relative biomass") +
  ggtitle(label = "Mean biomass by economic group for nine communities")
  

p2 <- ggplot(data = biomass, aes(x = community, y = biomass_kg_hect, color = economic)) + 
  geom_pointrange(aes(ymin = biomass_kg_hect - biomass_kg_hect_sd, ymax = biomass_kg_hect + biomass_kg_hect_sd), position = position_dodge(width = 0.5), shape = 21) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  guides(color = guide_legend("Value")) +
  theme_bw() +
  theme(legend.justification = c(1, 0),
        legend.position = c(1, 0)) +
  coord_flip() +
  labs(x = "Community", y = "Biomass (Kg / ha)") +
  ggtitle(label = "Mean biomass by economic group")


p3 <- prices %>% 
  mutate(family = fct_reorder(family, price)) %>% 
  ggplot(aes(x = family, y = price)) +
  geom_col(fill = "steelblue", color = "black") +
  coord_flip() +
  labs(x = "Taxonomic Family", y = "Price (MXN $ / Kg)") +
  ggtitle("Family prices") +
  theme_bw()

p4 <- biomass %>% 
  filter(economic == "Fisheries value") %>% 
  ggplot(aes(x = community, y = value_mxp_hect, fill = economic)) +
  geom_col(fill = "steelblue", color = "black") +
  coord_flip() +
  labs(x = "Taxonomic Family", y = "Value (MXN $ / Ha)") +
  theme_bw() + 
  ggtitle(label = "Economic value")


plot_grid(p3, p2, p4, ncol = 3, labels = "AUTO")
