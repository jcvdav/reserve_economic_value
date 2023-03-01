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
      caption = "Invertebrate taxa, reference sizes, and weight factors used to convert invertebrate abundance into biomass",
      col.names = c("Taxa", "Reference size (mm)", "Weight (gr / org)", "Source"),
      digits = 2,
      format = "latex",
      booktabs = T,
      escape = F) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  column_spec(column = 1, italic = T, width = "8em") %>% 
  column_spec(column = c(2, 3), width = "4em") %>% 
  column_spec(column = 4, width = "17em") %>% 
  save_kable(file = here("results", "tab", "tab_s2_invert_w_facts.tex"))
