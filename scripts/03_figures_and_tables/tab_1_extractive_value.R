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

# Load data ----------------------------------------------------------------
value_of_reserves <- readRDS(file = here("data", "output_data", "value_of_reserves.rds"))

## PROCESSING ##################################################################

percents <- value_of_reserves %>% 
  select(community, group, value, estimate) %>% 
  pivot_wider(names_from = value,
              values_from = estimate) %>% 
  mutate(pct = Difference / Absolute,
         pct = scales::percent(x = pct, accuracy = 0.01)) %>% 
  select(community, group, pct)
  
# Format the coefficients
table_data <- value_of_reserves %>% 
  select(community, group, value, estimate, std.error, p.val.stars) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(estimate = paste0(estimate, " (± ", std.error, ")", p.val.stars)) %>% 
  select(community, group, value, estimate) %>% 
  pivot_wider(names_from = value,
              values_from = estimate) %>% 
left_join(percents, by = c("community", "group"))

## BUILD AND EXPORT TABLE ######################################################
kable(x = table_data,
      label = "extractive_value",
      caption = "Sustainable extractive value (thousand MXP / ha) for marine
      reserves in each community. The columns with numeric values show the total
      value of biomass contained within the reserve, the historical minimum
      observed, and the extractive value (difference between total and
      historical). The last column shows the proportion of the total.
      Communities are ordered in ascending order based on the sum of their
      extractive value.",
      col.names = c("Community",
                    "Group",
                    "Total value",
                    "Historical min",
                    "Extractive value",
                    "Proportion"),
      format = "latex",
      linesep = "",
      booktabs = TRUE) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  column_spec(column = c(1),
              width = "6em") %>%
  collapse_rows(columns = 1) %>%
  save_kable(file = here("results", "tab", "tab_1_extractive_value.tex"))
