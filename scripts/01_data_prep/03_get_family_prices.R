######################################################
#title#
######################################################
#
# Purpose
#
######################################################


# Load libraries
library(here)
library(tidyverse)

drop_spp <-
  c(
    "ABULON",          
    "ALGAS",           
    "ALMEJA",          
    "ANCHOVETA",       
    "BAGRE",           
    "BANDERA",         
    "CALAMAR",         
    "CAMARON",         
    "CARACOL",         
    "CARPA",           
    "CAZON",           
    "CHARAL",          
    "CINTILLA",        
    "ERIZO",           
    "ESPECIE GENERICA", 
    "FAUNA",           
    "JAIBA",           
    "LANGOSTA",        
    "LANGOSTINO",      
    "LENGUADO",        
    "LOBINA",          
    "OSTION",          
    "OTRAS",           
    "PECES DE ORNATO", 
    "PEPINO DE MAR",   
    "PULPO",           
    "RAYA Y SIMILARES",
    "RUBIO",           
    "SARDINA",         
    "SARGAZO",         
    "TIBURON",         
    "TRUCHA"
  )

landings <-
  read_csv(here("data", "raw_data", "mex_anual_landings_by_vessel.csv")) %>%
  filter(!main_species_group %in% drop_spp)

prices <- landings %>%
  select(year_cut, main_species_group, landed_weight, value) %>%
  mutate(family = case_when(main_species_group == "LISA" ~ "Mugilidae",
                            main_species_group == "MOJARRA" ~ "Haemulidae",
                            main_species_group == "ROBALO" ~ "Centropomidae",
                            main_species_group == "SIERRA" ~ "Scombridae",
                            main_species_group == "GUACHINANGO" ~ "Lutjanidae",
                            main_species_group == "JUREL" ~ "Carangidae",
                            main_species_group == "PETO" ~ "Scombridae",
                            main_species_group == "BESUGO" ~ "Sparidae",
                            main_species_group == "CORVINA" ~ "Scianidae",
                            main_species_group == "PARGO" ~ "Lutjanidae",
                            main_species_group == "LEBRANCHA" ~ "Mugilidae",
                            main_species_group == "PIERNA" ~ "Malacanthidae",
                            main_species_group == "CABRILLA" ~ "Serranidae",
                            main_species_group == "MERO" ~ "Serranidae",
                            main_species_group == "RUBIA Y VILLAJAIBA" ~ "Lutjanidae",
                            main_species_group == "ESMEDREGAL" ~ "Rachycentridae",
                            main_species_group == "ATUN" ~ "Scombridae",
                            main_species_group == "RONCO" ~ "Haemulidae",
                            main_species_group == "PAMPANO" ~ "Carangidae",
                            main_species_group == "BERRUGATA" ~ "Scianidae",
                            main_species_group == "BARRILETE" ~ "Scombridae",
                            main_species_group == "BONITO" ~ "Scombridae",
                            main_species_group == "BAQUETA" ~ "Serranidae",
                            main_species_group == "MACARELA" ~ "Scombridae")) %>% 
drop_na(value) %>%
  drop_na(landed_weight) %>%
  mutate(price = value / landed_weight) %>%
  group_by(family) %>%
  summarize(price = mean(price, na.rm = T))

write_csv(x = prices,
          file = here("data", "processed_data", "family_prices.csv"))




