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
tot_val_ref <- readRDS(here("data", "output_data", "tot_val_ref.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
reference_dict <- tot_val_ref %>% 
  mutate(reference = paste0(zone, " (", year, ")")) %>% 
  select(community, group, reference) %>% 
  arrange(community, group)

## BUILD AND EXPORT TABLE #######################################################
kable(x = reference_dict,
      label = "ref_dict",
      caption = "Reference groups by community and group of species.",
      col.names = c("Community", "Group", "Reference point"),
      format = "latex",
      booktabs = T,
      escape = F) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  collapse_rows(columns = 1) %>% 
  save_kable(file = here("results", "tab", "tab_s3_ref_dict.tex"))
