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
# Price data
prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv"))
# Families
conapesca <- read_csv(here("data", "processed_data", "conapesca_families.csv"))
## PROCESSING ##################################################################

# Build table data -------------------------------------------------------------
 table_data <- conapesca %>% 
  left_join(prices, by = c("group", "family")) %>% 
  arrange(group, family) %>% 
  select(-median_price) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-sd_price) %>% 
  mutate(group = ifelse(group == "Escama", "Finfish", "Invertebrate")) %>% 
  group_by(group, family, mean_price) %>% 
  nest() %>% 
  mutate(conapesca = map_chr(data, ~paste(.x$conapesca, collapse = ", "))) %>% 
  select(group, family, conapesca, mean_price)

## BUILD AND EXPORT TABLE ######################################################

# X ----------------------------------------------------------------------------
kable(x = table_data,
      label = "conapesca_families",
      caption = "Mapping of general groups, taxonomic families, and CONAPESCA groups,
      along with mean ex-vessel price. CONAPESCA groups are shown in spanish to
      allow for direct comparison to the original data, but the reader should refer
      to the taxonomic family at all times.",
      col.names = c(
        "Group",
        "Family",
        "Group in CONAPESCA database",
        "Mean price (MXP / Kg)"),
      format = "latex",
      linesep = "",
      booktabs = TRUE)%>%
  kable_styling(latex_options = "HOLD_position") %>% 
  column_spec(column = c(1, 2, 4),
              width = "7em") %>% 
  column_spec(column = c(3),
              width = "14em") %>% 
  collapse_rows(columns = 1) %>% 
  save_kable(file = here("results", "tab", "tab_s1_conapesca_families.tex"))

