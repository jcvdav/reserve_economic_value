######################################################
#title#
######################################################
# 
# Purpose
#
######################################################


library(here)
library(tidyverse)

prices <-
  read_csv(file = here("data", "processed_data", "family_prices.csv"))

conapesca <- read_csv(here("data", "processed_data", "conapesca_families.csv")) %>% 
  left_join(prices, by = c("group", "family")) %>% 
  arrange(group, family) %>% 
  select(-median_price) %>% 
  mutate_if(is.numeric, round, 2)

knitr::kable(conapesca,
             format = "latex",
             booktabs = T,
             col.names = c("Grupo", "Familia", "Nombre (CONAPESCA)", "Precio promedio ($ / Kg)"),
             caption = "Grupos y familias asignados a los nombres de CONAPESCA, junto con el promedio de los precios unitarios.",
             label = "cona_fam") %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::collapse_rows(columns = c(1, 2, 4)) %>% 
  cat(file = here("results", "tab", "conapesca_families.tex"))


