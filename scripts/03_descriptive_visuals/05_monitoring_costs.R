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
library(here)
library(cowplot)
library(tidyverse)

# Load data --------------------------------------------------------------------
costs <-
  read.csv(here("data", "raw_data", "reserve_cost_per_hectare.csv")) %>%
  mutate(community = fct_reorder(community,-total))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

total_cost <- ggplot(
  data = costs,
  mapping = aes(x = community, y = total / 1000)) +
  geom_col(fill = "cadetblue", color = "black") +
  labs(x = "Community",
       y = "Annual cost (thousand MXP)") +
  theme_bw() +
  coord_flip()

norm_cost <- ggplot(
  data = costs,
  mapping = aes(x = community, y = cost_mxp_ha)) +
  geom_col(fill = "cadetblue", color = "black") +
  labs(x = "",
       y = "Normalized annual cost (MXP / ha)") +
  theme_bw() +
  coord_flip()

p <- plot_grid(total_cost, norm_cost, labels = "auto")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

ggsave(
  plot = p,
  filename = here("results", "img", "monitoriong_costs.png"),
  width = 8,
  height = 6
)
