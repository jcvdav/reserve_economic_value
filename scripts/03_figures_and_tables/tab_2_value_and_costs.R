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
  kableExtra,
  tidyverse
)

# Load data --------------------------------------------------------------------
# Sustainable extraction value data
value_of_reserves <- readRDS(file = here("data", "output_data", "value_of_reserves.rds"))
# Cost data
cost_of_reserves <-
  read.csv(here("data", "raw_data", "reserve_cost_per_hectare.csv"))

## PROCESSING ##################################################################

# Build data -------------------------------------------------------------------
values <- value_of_reserves %>% 
  filter(value == "Difference") %>% 
  group_by(community) %>% 
  summarize(value = sum(estimate, na.rm = T)) %>% 
  ungroup()

costs <- cost_of_reserves %>% 
  select(community, cost_MXN_ha)

table_data <- values %>% 
  left_join(costs, by = "community") %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(pct = (cost_MXN_ha / (value * 1e3)),
         pct = scales::percent(x = pct, accuracy = 0.01))

## BUILD AND EXPORT TABLE ######################################################
kable(table_data,
      label = "costs_and_benefits",
      caption = "Extractive value (summing value of invertebrate and fish biomass)
      and monitoring costs for reserves in each community. Note that value of
      biomass is presented in thousands of pesos per hectare, whil monitoring costs is
      in pesos per hectare.",
      col.names = c(
        "Community",
        "Extractive value (Thousand MXN / ha)",
        "Monitoring costs (MXN / ha)",
        "% of Value"),
      format = "latex",
      linesep = "",
      booktabs = TRUE)%>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  column_spec(column = c(1, 2, 3),
              width = "7em") %>%
  save_kable(file = here("results", "tab", "tab_2_costs_and_benefits.tex"))
