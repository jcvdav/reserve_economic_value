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
pacman::p_load(
  here,
  cowplot,
  tidyverse
)

# Source defaults --------------------------------------------------------------
source(here("scripts","_defaults.R"))

# Load data --------------------------------------------------------------------
costs <-
  read.csv(here("data", "raw_data", "reserve_cost_per_hectare.csv")) %>%
  mutate(community = fct_relevel(community, com_order))

## VISUALIZE ###################################################################

# Build a total costs plot -----------------------------------------------------
total_cost <- ggplot(
  data = costs,
  mapping = aes(x = community, y = total / 1000)) +
  geom_col(fill = "cadetblue", color = "black") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 500)) +
  labs(x = "Community",
       y = "Annual cost (thousand MXP)") +
  theme_bw() +
  coord_flip()

# Build a cost-per-area plot ---------------------------------------------------
norm_cost <- ggplot(
  data = costs,
  mapping = aes(x = community, y = cost_mxp_ha)) +
  geom_col(fill = "cadetblue", color = "black") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 4000)) +
  labs(x = "",
       y = "Normalized annual cost (MXP / ha)") +
  theme_bw() +
  coord_flip()

# Combine into a panel
p <- plot_grid(total_cost, norm_cost,
               labels = "auto")

## EXPORT ######################################################################
ggsave(
  plot = p,
  filename = here("results", "img", "fig_4_monitoring_costs.png"),
  width = 8,
  height = 6
)
