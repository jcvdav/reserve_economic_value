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

# SET UP #######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  cowplot,
  tidyverse 
)

# Load data --------------------------------------------------------------------
prices <-
  read_csv(file = here("data", "processed_data", "prices_ts.csv")) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate"))

family_prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv")) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate"))

## VISUALIZE ###################################################################

# Build individual panels showing the time series of each species --------------
ts_plot <-
  prices %>%
  mutate(family = fct_reorder(family, def_price, mean, .desc = T)) %>%
  ggplot(aes(
    x = year,
    y = def_price,
    color = group,
  )) +
  stat_summary(geom = "line",
               fun = "mean",
               linewidth = 0.5) +
  labs(x = "Year",
       y = bquote("Mean annual ex-vessel price (MXP"[2019] ~ "/ Kg)")) +
  theme(legend.position = "None") +
  guides(color = guide_legend("Family")) +
  scale_color_manual(values = palette) +
  facet_wrap(~family, ncol = 3, scales = "free_y")

# Now build a plot showing the summary -----------------------------------------
values_plot <- prices %>%
  mutate(family = fct_reorder(family, price, .fun = "mean")) %>%
  ggplot(aes(x = family, y = price, fill = group)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = palette) +
  labs(x = "Family",
       y = bquote("Mean ex-vessel price (MXP"[2019] ~ "/ Kg)")) +
  theme_bw() +
  guides(fill = guide_legend("Group")) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.background = element_blank()
  )

prices_panel <- plot_grid(ts_plot, values_plot,
                          ncol = 1,
                          rel_heights = c(1.5, 1),
                          labels = "auto")

# EXPORT PLOTS #################################################################
ggsave(
  plot = prices_panel,
  filename = here("results", "img", "fig_2_prices_panel.png"),
  width = 6,
  height = 8
)
