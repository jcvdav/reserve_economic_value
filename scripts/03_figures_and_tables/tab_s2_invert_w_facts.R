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
dens_weight_inverts <- read.csv(file = here("data", "processed_data",
                                            "invertebrate_weight_factors_citations.csv"))

kable(dens_weight_inverts,
      label = "w_fact",
      caption = "Weight factors used to convert invertebrate abundance into biomass",
      col.names = c("Family", "Weight (gr / org)", "Source"),
      digits = 2,
      format = "latex",
      booktabs = T,
      escape = F) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  save_kable(file = here("results", "tab", "tab_s2_invert_w_facts.tex"))
