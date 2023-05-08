################################################################################
# Clean fish and invertebrate transects
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Reads in the raw fish and invertebrate transects, cleans the data, and exports
#
################################################################################

## SET UP ######################################################################
# Load libraries
pacman::p_load(
  here,
  janitor,
  tidyverse
)

# DATA CLEANING ################################################################

# Fish transects ---------------------------------------------------------------
fish <-
  read_delim(here(                                                              # Read data
    "data",
    "raw_data",
    "transects",
    "Peces 2006-2019(24feb2021).csv"
  ),
  delim = ";") %>%
  janitor::clean_names() %>%
  select(                                                                       # Select relevant columns
    id = id,
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
    abundance = abundancia
  ) %>%
  filter(!str_detect(species, "Phoca|Zalophus"),                                # Remove sea lions and seals
         !community == "Isla Magdalena") %>%                                    # Remove community that cancelled their reserves
  mutate(
    size = as.numeric(ifelse(size == "ND", NA_character_, size)),
    abundance = as.numeric(ifelse(abundance == "ND", 0, abundance)),
    species = str_remove_all(species, "\xa0"),
    species = stringi::stri_enc_toutf8(species),
    species = str_to_sentence(species),
    species = str_trim(species),
    species = case_when(                                                        # Rename some species
      species == "Rhacochilus vacca" ~ "Phanerodon vacca",
      species == "Rhinobatos productus" ~ "Pseudobatos productus",
      species == "Dasyatis dipterura" ~ "Hypanus dipterurus",
      species == "Dasyatis americana" ~ "Hypanus americanus",
      species == "Damisela spp" ~ "Chromis spp",
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
      T ~ species
    )
  ) %>%
  mutate(                                                                       # Fix strings and site names
    site = str_replace_all(site, "\xf1", "n"),
    site = stringi::stri_enc_toutf8(site),
    site = str_to_sentence(site),
    site = str_remove_all(site, "Zrp"),
    site = str_remove_all(site, "\\(control\\)"),
    site = str_trim(site),
    site = case_when(
      site == "40canones2" ~ "40 canones",
      site == "40canones2control" ~ "40 canones",
      site == "40 canones control" ~ "40 canones",
      T ~ site
    ),
    zone = case_when(zone == "Pesca Parcial" ~ "Control",                       # Hommologate all reserves and controls
                     zone == "Pesca parcial langosta" ~ "Reserva",
                     T ~ zone)
  ) %>%
  mutate(
    community = case_when(
      community == "Guaymas" ~ "Isla San Pedro Nolasco",
      community == "Bahia de Kino" ~ "Isla San Pedro Mártir",
      T ~ community
    )
  ) %>%
  filter(!species == "Nd")


# Find all combinations of species occurring in each community to add zeroes
# Dictyionary of transects performed
fish_transect_by_com_year <- fish %>% 
  select(year, community, site, zone, transect, id) %>% 
  distinct()

# Dictionary of species and families ever recorded by community
fish_spp_by_com <- fish %>%
  filter(abundance > 0) %>% 
  select(community, species) %>%
  distinct()

# Observed abundances
fish_abundances <- fish %>% 
  filter(abundance > 0,
         !is.na(abundance)) %>%
  group_by(year, community, site, zone, transect, species, size) %>% 
  summarize(abundance = sum(abundance))

# Assemble full fish data-base
fish_completed <- fish_transect_by_com_year %>% 
  left_join(fish_spp_by_com, by = "community") %>% 
  left_join(fish_abundances,
            by = c("year", "community", "site", "zone", "transect", "species")) %>% 
  replace_na(replace = list(abundance = 0)) %>%
  mutate(genus = str_extract(species, "[:alpha:]+"),
         group = "Finfish")



# Invertebrate transects -------------------------------------------------------
inverts <-
  read_delim(
    here(
      "data",
      "raw_data",
      "transects",
      "Invertebrados 2006-2019(24feb2021).csv"
    ),
    delim = ";"
  ) %>%
  janitor::clean_names() %>%
  select(
    id = id,
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
    abundance = abundancia
  ) %>%
  filter(!community == "Isla Magdalena") %>%                                    # Filter out data from Magdalenda, whcih cancelled their reserves
  mutate(                                                                       # Fix strings on species data
    abundance = as.numeric(ifelse(abundance == "ND", 0, abundance)),
    species = str_remove_all(species, "\xa0"),
    species = stringi::stri_enc_toutf8(species),
    species = str_to_sentence(species),
    species = str_trim(species),
    species = str_squish(species)
  ) %>%
  mutate(                                                                       # Fix strings on site data
    site = str_replace_all(site, "\xf1", "n"),
    site = stringi::stri_enc_toutf8(site),
    site = str_to_sentence(site),
    site = str_remove_all(site, "Zrp"),
    site = str_remove_all(site, "\\(control\\)"),
    site = str_trim(site),
    site = str_squish(site),
    site = case_when(                                                           # Homologate sites
      site == "40 canones 2" ~ "40 canones",
      site == "40canones2control" ~ "40 canones",
      site == "40 canones control" ~ "40 canones",
      T ~ site
    )
  ) %>%
  mutate(
    community = case_when(
      community == "Guaymas" ~ "Isla San Pedro Nolasco",
      community == "Bahia de Kino" ~ "Isla San Pedro Mártir",
      T ~ community
    ),
    zone = case_when(zone == "Pesca Parcial" ~ "Reserva",
                     zone == "Pesca parcial langosta" ~ "Control",
                     T ~ zone)
  ) %>%
  mutate(
    family = case_when(
      str_detect(species, "Haliotis") ~ "Haliotidae",
      str_detect(species, "Panulirus") ~ "Palinuridae",
      species %in% c("Isostichopus fuscus",
                     "Holothuria impatiens",
                     "Parastichopus parvimensis", 
                     "Pepino spp", "Holothuroidea spp") ~ "Holothuroidea",      # All sea cucumber species
      species %in% c("Strongylocentrotus purpuratus",
                     "Mesocentrotus franciscanus") ~ "Strongylocentrotidae",          # All valualbe sea urchins
      T ~ NA_character_
    )
  ) %>% 
  filter(!species %in% c("Nd", "Pterois volitans"))                             # Remvoe unidentified species and lionfish

# Find all combinations of species occurring in each community to add zeroes
# Dictionary of transects performed
invert_transect_by_com_year <- inverts %>% 
  select(year, community, site, zone, transect, id) %>% 
  distinct()

# Dictionary of species and families ever recorded by community
invert_spp_by_com <- inverts %>%
  filter(abundance > 0) %>% 
  select(community, species, family) %>%
  distinct()

# Observed abundances
invert_abundances <- inverts %>% 
  filter(abundance > 0,
         !is.na(abundance)) %>%
  group_by(year, community, site, zone, transect, species) %>% 
  summarize(abundance = sum(abundance))

invert_completed <- invert_transect_by_com_year %>% 
  left_join(invert_spp_by_com, by = "community") %>% 
  left_join(invert_abundances,
            by = c("year", "community", "site", "zone", "transect", "species")) %>% 
  replace_na(replace = list(abundance = 0)) %>%
  mutate(genus = str_extract(species, "[:alpha:]+"),
         group = "Invertebrate")



# EXPORT DATA ##################################################################
# Export clean fish transects
write_csv(
  x = fish_completed,
  file = here("data", "processed_data", "clean_fish_transects.csv")             # Filename
)

# Export clean invertebrate transects
write_csv(
  x = invert_completed,
  file = here("data", "processed_data", "clean_invertebrate_transects.csv")     # Filename
)
