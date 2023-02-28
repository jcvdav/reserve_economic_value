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
  tidyverse
)

# Source defaults --------------------------------------------------------------
source(here("scripts","_defaults.R"))

# Load data --------------------------------------------------------------------
biomass_value <- readRDS(file = here("data", "output_data", "biomass_value_panel.rds"))


## PROCESSING ##################################################################
# Build a visualization data set -----------------------------------------------
vis_data <- biomass_value %>% 
  filter(year == 2019,
         zone == "Reserve")

## VISUALIZE ###################################################################
# Value of biomass by family ---------------------------------------------------
p <- ggplot(data = vis_data,
       mapping = aes(x = family, y = value_mxp_hect / 1e3, fill = group)) +
  stat_summary(geom = "col", fun = mean, color = "black", linewidth = 0.1) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  facet_wrap(~community, scales = "free_x", ncol = 3) +
  coord_flip() +
  scale_fill_manual(values = palette) +
  labs(x = "Taxonomic family",
       y = "Value of biomass (Thousand MXP / ha)",
       fill = "Group")

## EXPORT ######################################################################
ggsave(plot = p,
       filename = here("results", "img", "fig_s2_value_by_family.png"),
       width = 6.5,
       height = 5)
