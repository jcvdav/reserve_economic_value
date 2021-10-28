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
  left_join(prices, by = "family") %>% 
  replace_na(replace = list("mean_price" = 0,
                            "median_price" = 0)) %>% 
  mutate(economic = ifelse(mean_price > 0, "Fisheries value", "No fisheries value"),
         value_mxp_hect = median_price * biomass_kg_hect) 
  
biomass_by_site <- biomass_value %>% 
  group_by(year, community, site, zone, transect, economic) %>% 
  summarize(biomass_kg_hect = sum(biomass_kg_hect, na.rm = T),
            value_mxp_hect = sum(value_mxp_hect, na.rm = T)) %>% 
  group_by(community, site, economic) %>% 
  summarize(biomass_kg_hect_sd = sd(biomass_kg_hect, na.rm = T),
            biomass_kg_hect = mean(biomass_kg_hect, na.rm = T),
            value_mxp_hect_sd = sd(value_mxp_hect, na.rm = T),
            value_mxp_hect = mean(value_mxp_hect)) %>% 
  group_by(community, site) %>% 
  mutate(tot_biomass = sum(biomass_kg_hect),
         fishery_biomass = sum(biomass_kg_hect[economic == "Fisheries value"]) / tot_biomass) %>% 
  ungroup() %>% 
  mutate(community = fct_reorder(community, tot_biomass)) %>% 
  mutate(site = fct_reorder(site, fishery_biomass)) %>% 
  ungroup()
  
sites_in_com <- function(data, var) {
  ggplot(data = data,
         mapping = aes(x = site, y = {{var}}, fill = economic)) + 
    geom_col(color = "black") + 
    coord_flip() +
    scale_fill_brewer(palette = "Set1") +
    guides(fill = guide_legend("Value")) +
    theme_bw() +
    theme(legend.position = "None") +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    labs(x = "Community", y = "Relative biomass") +
    ggtitle(label = "Mean biomass by economic group for nine communities")
}



p1 <- ggplot(biomass_by_site, aes(x = site, y = biomass_kg_hect / tot_biomass, fill = economic)) + 
  geom_col(color = "black") + 
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend("Value")) +
  theme_bw() +
  # theme(legend.position = "None") +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  labs(x = "Community", y = "Relative biomass") +
  ggtitle(label = "Mean biomass by economic group for nine communities")


biomass_by_site %>% 
  mutate(b = paste0(round(biomass_kg_hect, 2), " (", round(biomass_kg_hect_sd, 2), ")")) %>% 
  select(community, site, b, economic, value_mxp_hect, fishery_biomass) %>% 
  pivot_wider(names_from = economic, values_from = c(b, value_mxp_hect, fishery_biomass)) %>% 
  select(-c(`value_mxp_hect_No fisheries value`, `fishery_biomass_No fisheries value`)) %>% 
  arrange(desc(community, `value_mxp_hect_Fisheries value`)) %>% 
  knitr::kable(col.names = c("Community", "Site", "Biomass (Fishery species)", "Biomass (Non-fishery species)", "Value", "Pct biomass with value")) %>% 
  kableExtra::collapse_rows(columns = 1)



  geom_pointrange(aes(ymin = biomass_kg_hect - biomass_kg_hect_sd, ymax = biomass_kg_hect + biomass_kg_hect_sd), position = position_dodge(width = 0.5), shape = 21) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  guides(color = guide_legend("Value")) +
  theme_bw() +
  theme(legend.justification = c(1, 0),
        legend.position = c(1, 0)) +
  coord_flip() +
  labs(x = "Community", y = "Biomass (Kg / ha)") +
  ggtitle(label = "Mean biomass by economic group")


p4 <- biomass_by_site %>% 
  filter(economic == "Fisheries value") %>% 
  ggplot(aes(x = community, y = value_mxp_hect, fill = economic)) +
  geom_col(fill = "steelblue", color = "black") +
  coord_flip() +
  labs(x = "Taxonomic Family", y = "Value (MXN $ / Ha)") +
  theme_bw() + 
  ggtitle(label = "Economic value")


plot_grid(p3, p2, p4, ncol = 3, labels = "AUTO")
