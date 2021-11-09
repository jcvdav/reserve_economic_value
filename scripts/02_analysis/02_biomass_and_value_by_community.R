######################################################
#title#
######################################################
#
# Purpose
#
######################################################

library(here)
library(ggrepel)
library(ggridges)
library(tidyverse)

fish_biomass <-
  read_csv(here("data", "processed_data", "fish_biomass_by_species.csv"))

invert_biomass <-
  read_csv(here( "data", "processed_data", "invertebrate_biomass_by_species.csv"))

family <-
  read_csv(file = here("data", "processed_data", "family_names.csv"))

prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv"))

costs <-
  read.csv(here("data", "raw_data", "reserve_cost_per_hectare.csv")) %>%
  mutate(cost_mxp_ha = cost_usd_ha * 19.2472) %>%
  select(community, cost_mxp_ha)

fish_biomass_value <- fish_biomass %>%
  left_join(family, by = c("species", "genus")) %>%
  left_join(prices, by = c("family", "group")) %>%
  mutate(value_mxp_hect = mean_price * biomass_kg_hect) %>%
  select(-c(genus, species))

invert_biomass_value <- invert_biomass %>%
  left_join(prices, by = c("family", "group")) %>%
  mutate(value_mxp_hect = mean_price * biomass_kg_hect)

biomass_value <- rbind(fish_biomass_value, invert_biomass_value)

biomass_by_transect <- biomass_value %>%
  drop_na(mean_price) %>%
  group_by(year, community, site, zone, group, transect) %>%
  summarize(
    biomass_kg_hect = sum(biomass_kg_hect, na.rm = T),
    value_mxp_hect = sum(value_mxp_hect, na.rm = T) / 1000)

biomass_by_community <- biomass_by_transect %>% 
  group_by(year, community, zone, group) %>%
  summarize(
    biomass_kg_hect_sd = sd(biomass_kg_hect, na.rm = T),
    biomass_kg_hect = mean(biomass_kg_hect, na.rm = T),
    value_mxp_hect_ci25 = quantile(value_mxp_hect, probs = 0.25, na.rm = T),
    value_mxp_hect_ci75 = quantile(value_mxp_hect, probs = 0.75, na.rm = T),
    value_mxp_hect = mean(value_mxp_hect)
  ) %>%
  ungroup() %>%
  mutate(community = fct_reorder(community, value_mxp_hect, sum))


# Valore de uso extractivo total
tot_val_data <- biomass_by_community %>%
  filter(zone == "Reserva",
         year == 2019)

# Valor inicial
tot_val_init <- biomass_by_community %>% 
  filter(zone == "Reserva") %>% 
  group_by(community) %>% 
  mutate(year_min = min(year)) %>% 
  ungroup() %>% 
  filter(year == year_min) %>% 
  select(community, group, Reserva_inicial = value_mxp_hect)

# Valor de uso sostenible
sust_val_data <- biomass_by_community %>%
  filter(year == 2019) %>% 
  select(-year) %>% 
  select(community, group, zone, value_mxp_hect) %>%
  pivot_wider(names_from = zone, values_from = value_mxp_hect) %>%
  left_join(tot_val_init, by = c("group", "community")) %>% 
  mutate(esc = pmin(Control, Reserva_inicial, na.rm = T),
         dif = pmax(0, Reserva - esc)) 


# FIGURES and TABLES ########################################################################################

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

tot_val_data %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(value = paste0(value_mxp_hect, " (", value_mxp_hect_ci25, " - ", value_mxp_hect_ci75, ")")) %>% 
  select(community, group, value) %>% 
  pivot_wider(names_from = group, values_from = value) %>% 
  arrange(desc(community)) %>% 
  knitr::kable(col.names = c("Comunindad", "Escama", "Invertebrados"),
               format = "latex",
               booktabs = T,
               caption = "Valor de las reservas por comunidad y grupo. Los valores presentan el promedio en miles de pesos del 2019 por hectárea. Los datos entre paréntesis indican los intervalos de confianza al 25\\% y 75\\%.",
               label = "tot_val") %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>% 
  cat(file = here("results", "tab", "total_value.tex"))


tot_val_dist <- biomass_by_transect %>%
  filter(zone == "Reserva", year == 2019) %>% 
  ggplot(aes(y = community, x = value_mxp_hect, fill = group)) +
  geom_density_ridges(bandwidth = 10, alpha = 0.75) +
  theme_bw() +
  labs(x = "Valor (1,000 MXP / ha)", y = "Comunidad") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  guides(fill = guide_legend("Grupo"))





# Sustainable value


sust_val <- ggplot(data = sust_val_data, 
                   aes(x = community, y = dif, fill = group)) +
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


sust_val_data %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(community, group, Reserva, Control, Reserva_inicial, dif) %>% 
  replace_na(replace = list(Reserva_inicial = 0)) %>% 
  group_by(community) %>% 
  mutate(a = sum(dif)) %>% 
  ungroup() %>% 
  arrange(desc(a)) %>% 
  select(-a) %>% 
  mutate(pct = paste0(round((dif / Reserva) * 100), "%")) %>% 
  knitr::kable(col.names = c("Comunidad", "Grupo", "Reserva", "Control", "Reserva (inicial)", "Valor", "%"),
               booktabs = TRUE,
               caption = "Valor de aprovechamiento sostenible (miles de pesos / ha) de las reservas marinas en cada comunidad. Las columnas de Reserva y Control muestran los valores para 2019. La columna de Reserva inicial muestra el valor de la reserva cuando fue implementada. La columna de Valor contiene la diferencia entre el valor de la reserva hoy y el valor del control hoy o la reserva en su implementación, cualquiera sea el menor. La última columna muestra el porcentaje del valor que puede ser aprovechado sosteniblemente, en relación a la extracción total. Las comunidades están ordenadas en orden descendiente según el valor sostenible total.",
               label = "sust_val",
               format = "latex") %>% 
  kableExtra::kable_styling(latex_options = "hold_position") %>% 
  kableExtra::collapse_rows(columns = 1) %>% 
  cat(file = here("results", "tab", "sust_value.tex"))

tot_sust_rel <- sust_val_data %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(community, group, Reserva, Control, Reserva_inicial, dif) %>% 
  replace_na(replace = list(Reserva_inicial = 0)) %>% 
  group_by(community) %>% 
  mutate(a = sum(dif)) %>% 
  ungroup() %>% 
  arrange(desc(a)) %>% 
  select(-a) %>% 
  mutate(pct = dif / Reserva * 100) %>% 
  ggplot(aes(x = Reserva, y = dif, size = pct)) +
  geom_point(shape = 21, aes(fill = group)) +
  theme_bw() +
  geom_label_repel(aes(label = community), size = 2) +
  labs(x = "Valor extractivo total (1,000 MXP / ha)", y = "Valor extractivo sostenible (1,000 MXP / ha)") +
  coord_equal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  lims(x = c(0, 125), y = c(0, 125)) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank()) +
  guides(fill = guide_legend("Grupo"),
         size = guide_legend("% extraible\nsosteniblemente"))



# Compared to cost
# 
sust_val_data %>% 
  group_by(community) %>% 
  summarize(value = sum(dif, na.rm = T) * 1000) %>% 
  left_join(costs, by = "community") %>%
  mutate(pct = (cost_mxp_ha / value) * 100) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  replace_na(list(cost_mxp_ha = 0, pct = 0)) %>%
  mutate(community = fct_reorder(community, pct),
         community = fct_relevel(community, "Isla San Pedro Mártir", after = Inf)) %>% 
  arrange(community) %>% 
  mutate(pct = ifelse(pct %in% c(0, Inf), "-", paste0(pct, "%"))) %>% 
  knitr::kable(
    col.names = c("Comunidad", "Valor (MXP / ha)", "Costo (MXP / ha)", "%"),
    booktabs = TRUE,
    format = "latex",
    caption = "Relación de valor de uso sostenible de las reservas y costos de operación.",
    label = "sust_val_cost"
  ) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>% 
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

ggsave(
  tot_sust_rel,
  filename = here("results", "img", "sustainable_vs_total.pdf"),
  width = 5,
  height = 5
)
