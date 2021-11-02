######################################################
#title#
######################################################
#
# Purpose
#
######################################################

# SET UP #######################################################################

# Load packages

# Load data


prices <-
  read_csv(file = here("data", "processed_data", "prices_ts.csv"))

family_prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv"))

ts_plot <-
  prices %>%
  mutate(family = fct_reorder(family, def_price, mean, .desc = T)) %>%
  ggplot(aes(
    x = year,
    y = def_price,
    color = family,
    linetype = group
  )) +
  stat_summary(fun = "mean", geom = "line", size = 1) +
  labs(x = "Año", y = bquote("Precio promedio ($"[2019] ~ "/ Kg)")) +
  theme_bw() +
  guides(color = guide_legend("Familia"),
         linetype = guide_legend("Grupo")) +
  scale_color_viridis_d()

values_plot <- family_prices %>%
  mutate(family = fct_reorder(family, mean_price)) %>%
  ggplot(aes(x = family, y = mean_price, fill = group)) +
  geom_col(color = "black") +
  geom_point(aes(y = median_price)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1", direction = 01) +
  labs(x = "Familia", y = "Precio ($ / Kg)") +
  theme_bw() +
  guides(fill = guide_legend("Grupo")) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.background = element_blank()
  )

# EXPORT PLOTS #################################################################
ggsave(
  plot = ts_plot,
  filename = here("results", "img", "family_prices_timeseries.pdf"),
  width = 10,
  height = 5.5
)

ggsave(
  plot = values_plot,
  filename = here("results", "img", "family_values.pdf"),
  width = 8,
  height = 4
)
