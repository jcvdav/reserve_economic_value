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
value_of_reserves <- readRDS(file = here("data", "output_data", "value_of_reserves.rds"))

## PROCESSING ##################################################################

# Build plotting data ----------------------------------------------------------

plot_data <- value_of_reserves %>% 
  filter(value == "Difference") %>% 
  select(community, group, estimate, std.error) %>% 
  pivot_wider(names_from = group, values_from = c(estimate, std.error))

## VISUALIZE ###################################################################

# Build plot -------------------------------------------------------------------
fish_inv <- ggplot(data = plot_data,
       mapping = aes(x = estimate_Finfish,
                     y = estimate_Invertebrate,
                     fill = community)) +
  geom_abline(intercept = 0,
              linetype = "dashed") +
  geom_errorbarh(mapping = aes(xmin = estimate_Finfish - std.error_Finfish,
                               xmax = estimate_Finfish + std.error_Finfish),
                 height = 0) +
  geom_errorbar(mapping = aes(ymin = estimate_Invertebrate - std.error_Invertebrate,
                              ymax = estimate_Invertebrate + std.error_Invertebrate),
                width = 0) +
  geom_point(shape = 21,
             size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Extractive value of finfish biomass\n(Thousand MXN / ha)",
       y = "Extractive value of invertebrate biomass\n(Thousand MXN / ha)",
       fill = "Community") 
  

## EXPORT ######################################################################
ggsave(plot = fish_inv,
       file = here("results", "img", "fig_6_fish_vs_invert.png"),
       width = 6,
       height = 4)
