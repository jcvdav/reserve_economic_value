######################################################
#title# 
######################################################
#
# Purpose
#
######################################################

## SET UP ######################################################################
# Load libraries
library(here)
library(cowplot)
library(ggrepel)
library(ggridges)
library(tidyverse)

# Load data
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
  mutate(cost_mxp_ha = cost_usd_ha * 19) %>%
  select(community, cost_mxp_ha)

## PROCESSING ##################################################################

fish_biomass_value <- fish_biomass %>%
  left_join(prices, by = c("family", "group")) %>%
  mutate(value_mxp_hect = mean_price * biomass_kg_hect)

invert_biomass_value <- invert_biomass %>%
  left_join(prices, by = c("family", "group")) %>%
  mutate(value_mxp_hect = mean_price * biomass_kg_hect)

biomass_value <- rbind(fish_biomass_value, invert_biomass_value) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate"),
         zone = str_replace(zone, "Reserva", "Reserve"))

biomass_by_transect <- biomass_value %>%
  drop_na(mean_price) %>%
  group_by(year, community, site, zone, group, transect) %>%
  summarize(
    biomass_kg_hect = sum(biomass_kg_hect, na.rm = T),
    value_mxp_hect = sum(value_mxp_hect, na.rm = T) / 1000) %>% 
  ungroup()

biomass_by_community <- biomass_by_transect %>% 
  group_by(year, community, zone, group) %>%
  summarize(
    biomass_kg_hect_sd = sd(biomass_kg_hect),
    biomass_kg_hect = mean(biomass_kg_hect),
    value_mxp_hect_ci_low = quantile(value_mxp_hect, probs = 0.025),
    value_mxp_hect_ci_high = quantile(value_mxp_hect, probs = 0.975),
    sd_value_mxp_hect = sd(value_mxp_hect),
    value_mxp_hect = mean(value_mxp_hect),
    n_obs = n(),
    std.error = sd_value_mxp_hect / sqrt(n_obs - 1)
  ) %>%
  ungroup() 



## Visualization datasets ------------------------------------------------------


# Valor en 2019
tot_val_data <- biomass_by_community %>%
  filter(zone == "Reserve",
         year == 2019) %>% 
  mutate(id2 = paste(community, group, zone, year, sep = "-"),
         type = "Latest")

# # Valor inicial
# tot_val_init <- biomass_by_community %>% 
#   filter(zone == "Reserva") %>% 
#   group_by(community) %>% 
#   mutate(year_min = min(year)) %>% 
#   ungroup() %>% 
#   filter(year == year_min) %>% 
#   select(community, group, Reserva_inicial = value_mxp_hect)

# Mínimos históricos
# Identificar el mínimo historico
tot_val_ref <- biomass_by_community %>% 
  group_by(community) %>% 
  mutate(year_min = min(year)) %>% 
  ungroup() %>% 
  filter((year < 2019 & zone == "Control") | (zone == "Reserve" & year == year_min)) %>% 
  filter(value_mxp_hect > 0) %>%
  group_by(community, group) %>% 
  mutate(val_min = min(value_mxp_hect)) %>% 
  ungroup() %>% 
  filter(value_mxp_hect == val_min) %>% 
  select(community, group, zone, year, value_mxp_hect) %>% 
  arrange(community, group) %>% 
  mutate(id2 = paste(community, group, zone, year, sep = "-"),
         type = "Reference")

# Extraer el identidicador de cada mínio
references <- tot_val_ref$id2

# Datos para analisar el mínimo histórico
trans <- biomass_by_transect %>% 
  mutate(id2 = paste(community, group, zone, year, sep = "-")) %>% 
  filter((zone == "Reserve" & year == 2019) | id2 %in% references)


# <- biomass_by_community %>%
#   filter(year == 2019) %>%
#   select(-year) %>%
#   select(community, group, zone, value_mxp_hect) %>%
#   pivot_wider(names_from = zone, values_from = value_mxp_hect) %>%
#   left_join(tot_val_init, by = c("group", "community")) %>%
#   mutate(esc = pmin(Control, Reserva_inicial, na.rm = T),
#          dif = pmax(0, Reserva - esc),
#          dif2 = pmax(0, Reserva - Control))


# ANALYSIS #####################################################################

my_lm <- function(x) {
  fixest::feols(value_mxp_hect ~ treatment, x)
}

models <- trans %>% 
  mutate(treatment = (zone == "Reserve" & year == 2019)) %>% 
  arrange(community, group) %>% 
  group_by(community, group) %>% 
  nest() %>% 
  mutate(mod2 = map(data, my_lm))

m <- models$mod2
names(m) <- paste(models$community, models$group)

modelsummary::modelsummary(m,
                           stars = T,
                           statistic = "std.error",
                           gof_omit = "Adj|Wi|Lik|IC|Ps|Std.",
                           vcov = "robust",
                           output = here("results", "tab", "reg_table.docx"))


# FIGURES and TABLES ###########################################################
# Time series ------------------------------------------------------------------
value_ts <- ggplot(data = biomass_by_community,
       mapping = aes(x = lubridate::ymd(paste(year, "01", "01", sep = "-")), y = value_mxp_hect, color = group, fill = group, linetype = zone)) +
  geom_line() +
  geom_errorbar(aes(ymin = value_mxp_hect - std.error, ymax = value_mxp_hect + std.error),
                width = 0, color = "black", linetype = "solid") +
  geom_point() +
  geom_point(data = tot_val_ref, pch = 21, fill = "transparent", size = 4) +
  facet_wrap( ~ community, scales = "free") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom") +
  labs(x = "Year",
       y = "Value (Thousand MXP / ha)",
       linetype = "Zone",
       fill = "Group",
       color = "Group")

ggsave(plot = value_ts,
       filename = here("results", "img", "value_timeseries.png"),
       width = 6.5,
       height = 5)

# Total value
tot_val <-
  ggplot(data = tot_val_data,
         mapping = aes(x = community, y = value_mxp_hect, fill = group)) +
  geom_col(color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = value_mxp_hect - std.error,
                    ymax = value_mxp_hect + std.error),
                position = "dodge") +
  scale_y_continuous(limits = c(NA, 80)) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  coord_flip() +
  labs(x = "Comunity",
       y = "Value of biomass\n(Thousands Thousand MXP / ha)",
       fill = "Group") +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.background = element_blank()
  )

# Sustainable value
sust_val <- models %>% 
  select(community, group, mod2) %>% 
  mutate(mod1 = map(mod2, broom::tidy)) %>% 
  unnest(mod1) %>% 
  filter(term == "treatmentTRUE") %>% 
  mutate(estimate = pmax(estimate, 0),
         std.error = (estimate > 0) * std.error) %>%
  ggplot(aes(x = community, y = estimate, fill = group)) +
  geom_col(position = "dodge",
           color = "black") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                position = "dodge") +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(NA, 80)) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  coord_flip() +
  labs(x = "", y = "Extractive value of biomass\n(Thousands Thousand MXP / ha)") +
  theme(legend.position = "None")

p <- plot_grid(tot_val, sust_val,
               ncol = 2,
               rel_widths = c(1.45, 1),
               labels = "auto")

ggsave(plot = p,
       filename = here("results", "img", "economic_values.png"),
       width = 7, height = 6)

# Invert vs Finfish
fish_inv <- models %>% 
  select(community, group, mod2) %>% 
  mutate(mod1 = map(mod2, broom::tidy)) %>% 
  unnest(mod1) %>% 
  filter(term == "treatmentTRUE") %>% 
  mutate(estimate = pmax(estimate, 0),
         std.error = (estimate > 0) * std.error) %>% select(community, group, estimate, std.error) %>% 
  pivot_wider(names_from = group, values_from = c(estimate, std.error)) %>%
  ggplot(aes(x = estimate_Finfish,
             y = estimate_Invertebrate,
             fill = community)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = estimate_Invertebrate - std.error_Invertebrate,
                    ymax = estimate_Invertebrate + std.error_Invertebrate),
                width = 0.5) +
  geom_errorbarh(aes(xmin = estimate_Finfish - std.error_Finfish,
                     xmax = estimate_Finfish + std.error_Finfish),
                 height = 0.5) +
  geom_point(shape = 21, size = 3) +
  scale_fill_brewer(palette = "Set1") +
  coord_equal() +
  labs(x = "Extractive value of finfish biomass\n(Thousand MXP / ha)",
       y = "Extractive value of invertebrate biomass\n(Thousand MXP / ha)",
       fill = "Community") +
  theme_bw()

ggsave(plot = fish_inv,
       file = here("results", "img", "fish_vs_invert.png"),
       width = 5,
       height = 4)

# Table

# Valor de uso sostenible
sust_val_data <- biomass_by_community %>% 
  mutate(id2 = paste(community, group, zone, year, sep = "-")) %>% 
  filter((zone == "Reserve" & year == 2019) | id2 %in% references) %>% 
  mutate(type = ifelse(id2 %in% references, "Reference", "Latest")) %>% 
  select(community, group, type, value_mxp_hect, std.error, n_obs, sd_value_mxp_hect) %>%
  pivot_wider(names_from = type, values_from = c(value_mxp_hect, std.error, n_obs, sd_value_mxp_hect)) %>%
  mutate(dif = pmax(0, value_mxp_hect_Latest - value_mxp_hect_Reference),
         std.error_dif = sqrt(((sd_value_mxp_hect_Reference ^ 2) / (n_obs_Reference - 1)) + ((sd_value_mxp_hect_Latest ^ 2) / (n_obs_Latest - 1))),
         pct = dif / value_mxp_hect_Latest * 100) 

sust_val_data_tab <- sust_val_data %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(reference = paste0(round(value_mxp_hect_Reference, 2), " (", round(std.error_Reference, 2), ")"),
         latest = paste0(round(value_mxp_hect_Latest, 2), " (", round(std.error_Latest, 2), ")"),
         difference = paste0(round(dif, 2), " (", round(std.error_dif, 2), ")"),
         pct = paste0(pct, "%"),
         difference = ifelse(dif == 0, "-", difference),
         pct = ifelse(dif == 0, "-", pct)) %>%
  arrange(community, group) %>% 
  select(community, group, latest, reference, difference, pct)


# Extract difference in models here, and add to the table above

# sust_val_data_tab <- sust_val_data %>% 
#   mutate_if(is.numeric, round, 2) %>% 
#   select(community, group, Reserve = Latest, Reference, dif, pct) %>% 
#   mutate(pct = paste0(round(pct, 2), "%"))

write_csv(x = sust_val_data_tab,
          file = here("results", "tab", "extractive_value_table.csv"))


# Compared to cost
# 
cost_vs_ben <- sust_val_data %>% 
  group_by(community) %>% 
  summarize(value = sum(dif, na.rm = T)) %>% 
  left_join(costs, by = "community") %>%
  mutate(pct = ((cost_mxp_ha / 1e3) / value) * 100) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  replace_na(list(cost_mxp_ha = 0, pct = 0)) %>%
  # mutate(community = fct_reorder(community, pct),
  #        community = fct_relevel(community, "Isla San Pedro Mártir", after = Inf)) %>% 
  arrange(community) %>% 
  mutate(pct = ifelse(pct %in% c(0, Inf), "-", paste0(pct, "%")))


write_csv(x = cost_vs_ben,
          file = here("results", "tab", "value_and_costs.csv"))



# Dictionary showing what is used as reference point
reference_dict <- biomass_by_community %>% 
  select(community, group) %>% 
  distinct() %>% 
  left_join(tot_val_ref, by = c("community", "group")) %>% 
  mutate(reference = paste0(zone, " (", year, ")")) %>% 
  select(community, group, reference) %>% 
  arrange(community, group)

write_csv(x = reference_dict,
          file = here("results", "tab", "reference_dictionary.csv"))









## SUPPLEMENTRY FIGURES




# Value of biomass by family
biomass_value %>% 
  filter(year == 2019,
         zone == "Reserve") %>% 
  group_by(community, group, family) %>% 
  summarize(mean_value = mean(value_mxp_hect, na.rm = T),
            ci_low = quantile(value_mxp_hect, probs = 0.025, na.rm = T),
            ci_high = quantile(value_mxp_hect, probs = 0.975, na.rm = T)) %>% 
  ggplot(aes(x = family, y = mean_value / 1e3, fill = group)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = ci_low / 1e3, ymax = ci_high / 1e3),
                width = 0.5) +
  facet_wrap(~community, scales = "free_x", ncol = 3) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Taxonomic family",
       y = "Value of biomass (Thousand MXP / ha)",
       fill = "Group") +
  theme_bw()






# This shows that Banco Chinchorro only has one transect with invert biomass in the control site of 2019. It shuld have more, even with zeroes.

filter(
  invert_biomass,
  community == "Banco Chinchorro",
  group == "Invertebrado",
  zone == "Control",
  year == 2019
)

filter(
  invert_dens,
  community == "Banco Chinchorro",
  group == "Invertebrado",
  zone == "Control",
  year == 2019
)



filter(
  clean_inverts,
  community == "Banco Chinchorro",
  group == "Invertebrado",
  zone == "Control",
  year == 2019
) %>% 
  View()






# Reference value plot


