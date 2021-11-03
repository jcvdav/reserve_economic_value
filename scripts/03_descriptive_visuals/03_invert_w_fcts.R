######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(tidyverse)

dens_weight_inverts <- read.csv(here("data", "raw_data", "invertebrate_weight_factors.csv"))

knitr::kable(dens_weight_inverts,
             format = "latex",
             label = "w_fact",
             caption = "Factores de masa por individuo para invertebrados.",
             col.names = c("Familia", "Peso (gr / ind)", "Fuente"),
             booktabs = T, escape = F) %>% 
  cat(file = here("results", "tab", "weight_facts.tex"))
