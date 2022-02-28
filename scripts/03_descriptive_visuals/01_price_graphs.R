######################################################
#title#
######################################################
#
# Purpose
#
######################################################

# SET UP #######################################################################

# Load packages
library(here)
library(cowplot)
library(tidyverse)

# Load data
prices <-
  read_csv(file = here("data", "processed_data", "prices_ts.csv")) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate"))

family_prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv")) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate"))

ts_plot <-
  prices %>%
  mutate(family = fct_reorder(family, def_price, mean, .desc = T)) %>%
  ggplot(aes(
    x = year,
    y = def_price,
    color = group,
  )) +
  stat_summary(fun = "mean", geom = "line") +
  labs(x = "Year",
       y = bquote("Mean annual ex-vessel price (MXP"[2019] ~ "/ Kg)")) +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank(), strip.text = element_text(size = 8)) +
  guides(color = guide_legend("Family")) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~family, ncol = 3, scales = "free_y")




values_plot <- family_prices %>%
  mutate(family = fct_reorder(family, mean_price)) %>%
  ggplot(aes(x = family, y = mean_price, fill = group)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean_price - sd_price, ymax = mean_price + sd_price),
                width = 0.5) +
  geom_point(aes(y = median_price)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1", direction = 01) +
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
  filename = here("results", "img", "prices_panel.png"),
  width = 6,
  height = 8
)
