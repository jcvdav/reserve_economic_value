################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Clea nthe consumer prince index data
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  janitor,
  tidyverse
)

# Load data --------------------------------------------------------------------
cpi_all <- read_csv(here("data", "raw_data", "CPI_all.csv"), show_col_types = F) %>% 
  clean_names()

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
cpi_mex <- cpi_all %>% 
  filter(location == "MEX",
         indicator == "CPI",
         subject == "FOOD",
         measure == "AGRWTH",
         frequency == "A") %>% 
  select(year = time, cpi = value) %>% 
  mutate(fact = cpi / cpi[year == 2019])

## EXPORT ######################################################################
write_csv(cpi_mex, here("data", "processed_data", "mex_cpi.csv"))
