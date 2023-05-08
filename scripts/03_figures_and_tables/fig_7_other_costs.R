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
cost_data <- read_csv(here("data", "raw_data", "reserve_cost_per_hectare.csv"))

## PROCESSING ##################################################################
plot_data <- cost_data %>% 
  mutate(source = "Community-basde MPAs (Our estimates)") %>% 
  select(source, cost_MXN_ha) %>% 
  bind_rows(tibble(source = c("Top-down MPAs (Balmford et al., 204)",
                              "Top-down MPAs: Mexico (Hayashida et al., 2021)"),
                   cost_MXN_ha = c(155, 0.17))) %>% 
  mutate(source = fct_reorder(source, -cost_MXN_ha, mean))

range_fx <- function(x) {
  tibble(y = median(x),
         ymin = min(x),
         ymax = max(x))
}

## VISUALIZE ###################################################################
costs <- ggplot(data = plot_data,
                mapping = aes(x = source, y = cost_MXN_ha)) +
  stat_summary(geom = "col",
               fun = median,
               color = "black",
               fill = "cadetblue") +
  stat_summary(geom = "errorbar",
               fun.data = range_fx,
               color = "black",
               width = 0) +
  labs(x = "Source",
       y = "Cost (MXN/ha)")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
ggsave(plot = costs, 
       filename = here("results", "img", "fig_7_other_costs.png"),
       width = 8, height = 4)
