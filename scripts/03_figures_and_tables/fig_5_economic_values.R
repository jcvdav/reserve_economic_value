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

## VISUALIZE ###################################################################
p <- ggplot(value_of_reserves %>% 
              filter(!value == "Historical"),
            aes(x = estimate, y = community, fill = group)) +
  geom_col(position = "dodge",
           color = "black",
           linewidth = 0.2) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error),
                 position = position_dodge(width = 1),
                 height = 0) +
  geom_text(aes(x = estimate + std.error + 5, label = p.val.stars),
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = palette) +
  facet_wrap(~type) +
  theme(legend.position = c(1,1),
        legend.justification = c(1, 1),
        axis.title.y = element_blank()) +
  labs(x = "Value of biomass (Thousand MXN / ha)",
       fill = "Group")

## EXPORT ######################################################################
ggsave(plot = p,
       filename = here("results", "img", "fig_5_economic_values.png"),
       width = 7, height = 6)
