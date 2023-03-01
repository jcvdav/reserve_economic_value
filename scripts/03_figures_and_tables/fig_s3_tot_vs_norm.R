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
tot_vs_norm <- ggplot(data = costs,
                      aes(x = total / 1e3,
                          y = cost_MXN_ha,
                          size = ha,
                          fill = community)) +
  geom_point(shape = 21) +
  scale_fill_brewer(palette = "Set1") +
  # theme(legend.justification = c(1, 1),
  # legend.position = "bottom") +
  labs(x = "Annual cost (thousand MXN)",
       y = "Normalized annual cost (MXN / ha)",
       fill = "Community",
       size = "Total reserve\nsize (ha)")

ggsave(
  plot = tot_vs_norm,
  filename = here("results", "img", "fig_s3_tot_vs_norm.png"),
  width = 8,
  height = 5
)

