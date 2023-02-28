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
tot_val_ref <- readRDS(here("data", "output_data", "tot_val_ref.rds"))

biomass_by_transect <- readRDS(here("data", "output_data", "summarized_biomass_value_by_transect.rds"))

## VISUALIZE ###################################################################

# Time series ------------------------------------------------------------------
value_ts <- ggplot(data = biomass_by_transect,
                   mapping = aes(x = year,
                                 y = value_mxp_hect,
                                 color = group,
                                 fill = group,
                                 linetype = zone)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0, color = "black") +
  stat_summary(geom = "point", fun = mean) +
  stat_summary(geom = "line", fun = mean) +
  geom_point(data = tot_val_ref,
             aes(y = ref_value_mxp_hect, color = group),
             pch = 21,
             fill = "transparent",
             size = 4) +
  facet_wrap( ~ community, scales = "free") +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme(legend.position = "bottom") +
  labs(x = "Year",
       y = "Value (Thousand MXP / ha)",
       linetype = "Zone",
       fill = "Group",
       color = "Group")

## EXPORT ######################################################################
ggsave(plot = value_ts,
       filename = here("results", "img", "fig_3_value_timeseries.png"),
       width = 6.5,
       height = 5)
