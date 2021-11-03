######################################################
#title#
######################################################
#
# Purpose
#
######################################################

library(here)
library(tidyverse)

fish_biomass <-
  read_csv(here("data", "processed_data", "fish_biomass_by_species.csv"))
invert_biomass <-
  read_csv(here(
    "data",
    "processed_data",
    "invertebrate_biomass_by_species.csv"
  ))
family <-
  read_csv(file = here("data", "processed_data", "family_names.csv"))
prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv"))
costs <-
  read.csv(here("data", "raw_data", "reserve_cost_per_hectare.csv")) %>%
  mutate(cost_mxp_ha = cost_usd_ha * 19.2472) %>%
  select(community, cost_mxp_ha)

fish_biomass_value <- fish_biomass %>%
  # filter(biomass_kg_hect > 0) %>%
  left_join(family, by = c("species", "genus")) %>%
  left_join(prices, by = c("family", "group")) %>%
  mutate(value_mxp_hect = mean_price * biomass_kg_hect) %>%
  select(-c(genus, species))

invert_biomass_value <- invert_biomass %>%
  # filter(biomass_kg_hect > 0) %>%
  left_join(prices, by = c("family", "group")) %>%
  mutate(value_mxp_hect = mean_price * biomass_kg_hect)

biomass_value <- rbind(fish_biomass_value, invert_biomass_value)

biomass_by_transect <- biomass_value %>%
  drop_na(mean_price) %>%
  group_by(community, site, zone, group, transect) %>%
  summarize(
    biomass_kg_hect = sum(biomass_kg_hect, na.rm = T),
    value_mxp_hect = sum(value_mxp_hect, na.rm = T) / 1000)

biomass_by_community <- biomass_by_transect %>% 
  group_by(community, zone, group) %>%
  summarize(
    biomass_kg_hect_sd = sd(biomass_kg_hect, na.rm = T),
    biomass_kg_hect = mean(biomass_kg_hect, na.rm = T),
    value_mxp_hect_ci25 = quantile(value_mxp_hect, probs = 0.25, na.rm = T),
    value_mxp_hect_ci75 = quantile(value_mxp_hect, probs = 0.75, na.rm = T),
    value_mxp_hect = mean(value_mxp_hect)
  ) %>%
  ungroup() %>%
  mutate(community = fct_reorder(community, value_mxp_hect, sum))


tot_val_data <- biomass_by_community %>%
  filter(zone == "Reserva")



# FIGURES

# Total value

tot_val <-
  ggplot(data = tot_val_data,
         mapping = aes(x = community, y = value_mxp_hect, fill = group)) +
  geom_col(color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = value_mxp_hect - value_mxp_hect_ci25,
                    ymax = value_mxp_hect + value_mxp_hect_ci75),
                position = "dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  theme_bw() +
  coord_flip() +
  labs(x = "Comunidad", y = "Valor (1,000 MXP / ha)") +
  guides(fill = guide_legend("Grupo")) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.background = element_blank()
  )


tot_val_dist <- biomass_by_transect %>%
  filter(zone == "Reserva") %>% 
  ggplot(aes(y = community, x = value_mxp_hect, fill = group)) +
  geom_density_ridges(bandwidth = 10, alpha = 0.75) +
  theme_bw() +
  labs(x = "Valor (1,000 MXP / ha)", y = "Comunidad") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  guides(fill = guide_legend("Grupo"))





# Sustainable value

sust_val <- biomass_by_community %>%
  select(community, group, zone, value_mxp_hect) %>%
  pivot_wider(names_from = zone, values_from = value_mxp_hect) %>%
  mutate(res = pmin(Control, 0.5 * Reserva),
         dif = Reserva - res) %>%
  ggplot(aes(x = community, y = dif, fill = group)) +
  geom_col(color = "black", position = "dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  theme_bw() +
  coord_flip() +
  labs(x = "Comunidad", y = "Valor (1,000 MXP / ha)") +
  guides(fill = guide_legend("Grupo")) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.background = element_blank()
  )



# Compared to cost
# 
biomass_by_community %>%
  select(community, group, zone, value_mxp_hect) %>%
  pivot_wider(names_from = zone, values_from = value_mxp_hect) %>%
  mutate(Res50 = 0.5 * Reserva,
         diff = Reserva - min(Control, Res50)) %>%
  group_by(community) %>%
  summarize(value = sum(diff)) %>%
  left_join(costs, by = "community") %>%
  knitr::kable(
    col.names = c("Comunidad", "Valor (MXP / hac)", "Costo (MXP / ha)"),
    format = "latex"
  ) %>%
  cat(file = here("results", "tab", "valor_y_costo.tex"))


# Export figures

ggsave(
  tot_val,
  filename = here("results", "img", "total_value_per_hectare.pdf"),
  width = 8,
  height = 4
)

ggsave(
  tot_val_dist,
  filename = here("results", "img", "distribution_total_value_per_hectare.pdf"),
  width = 8,
  height = 4
)

ggsave(
  sust_val,
  filename = here("results", "img", "sustainable_value_per_hectare.pdf"),
  width = 8,
  height = 4
)
