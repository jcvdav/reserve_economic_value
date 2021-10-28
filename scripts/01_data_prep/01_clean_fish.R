######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

# Load libraries
library(here)
library(janitor)
library(tidyverse)

# Load data

fish <- read_delim(here("data", "raw_data", "transects", "Peces 2006-2019(24feb2021).csv"), delim = ";") %>% 
  janitor::clean_names() %>% 
  select(id = id, 
         year = anio, 
         state = estado, 
         community = comunidad, 
         site = sitio, 
         diver = monitor,
         lat = latitud, 
         lon = longitud, 
         habitat = habitat, 
         zone = zona, 
         protection = proteccion, 
         mpa = anp, 
         transect = transecto, 
         species = especie, 
         size = talla, 
         abundance = abundancia) %>% 
  filter(!str_detect(species, "Phoca|Zalophus")) %>% 
  mutate(size = as.numeric(ifelse(size == "ND", NA_character_, size)),
         abundance = as.numeric(ifelse(abundance == "ND", 0, abundance)),
         species = str_remove_all(species, "\xa0"),
         species = stringi::stri_enc_toutf8(species),
         species = str_to_sentence(species),
         species = str_trim(species),
         species = case_when(species == "Damisela spp" ~ "Chromis spp",
                             species == "Lancero spp" ~ "Acanthurus spp",
                             species == "Loro spp" ~ "Scarus spp",
                             species == "Mariposa spp" ~ "Chaetodon spp",
                             species == "Pluma spp" ~ "Calamus spp",
                             species == "Vieja spp" ~ "Lachnolaimus maximus",
                             species == "Xcochin spp" ~ "Balistes spp",
                             species == "Kyphosus sectatrix/incisor" ~ "Kyphosus spp",
                             species == "Pterois volitans/miles" ~ "Pterois volitans",
                             species == "Macrostomum melanurum" ~ "Haemulon macrostomum",
                             species == "Xenistius californiensis" ~ "Haemulon californiensis",
                             T ~ species)) %>% 
  mutate(site = str_replace_all(site, "\xf1", "n"),
         site = stringi::stri_enc_toutf8(site),
         site = str_to_sentence(site),
         site = str_remove_all(site, "Zrp"),
         site = str_remove_all(site, "norte|sur|\\(control\\)"),
         site = str_trim(site),
         site = case_when(site == "40canones2" ~ "40 canones",
                          site == "40canones2control" ~ "40 canones",
                          site == "40 canones control" ~ "40 canones",
                          T ~ site)) %>% 
  filter(!species == "Nd") %>% 
  mutate(genus = str_extract(species, "[:alpha:]+"))


write_csv(x = fish, file = here("data", "processed_data", "clean_fish_transects.csv"))















