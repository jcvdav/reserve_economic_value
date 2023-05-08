################################################################################
# Get fish alometric parameters
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Collects allometric parameters from MPAtools and FishBase
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  rfishbase,
  janitor,
  tidyverse  
)

options(FISHBASE_VERSION = "23.01")

# Load data --------------------------------------------------------------------
# Inverts
clean_fish <-
  read_csv(file = here("data", "processed_data", "clean_fish_transects.csv"))

## PROCESS #####################################################################

# Get other parameters from fishbase -------------------------------------------
# SPecies list
spp_list <- clean_fish %>% 
  pull(species) %>% 
  unique() %>% 
  sort()

# Get data from FishBase
alo_spp <- length_weight(species_list = spp_list) %>% 
  clean_names() %>% 
  select(species, a, b) %>% 
  group_by(species) %>% 
  summarize(a = mean(a, na.rm = T),
            b = mean(b, na.rm = T)) %>% 
  select(species, a, b)

# Get bayesian estimates
alo_bayes <- estimate(species_list = spp_list) %>% 
  clean_names() %>% 
  select(species, a_bayes = a, b_bayes = b)

# Combine measured and estimated values
alo_spp_bayes <- alo_spp %>% 
  left_join(alo_bayes, by = c("species")) %>% 
  mutate(a = coalesce(a, a_bayes),
         b = coalesce(b, b_bayes)) %>% 
  select(species, a, b)

# Calculate at the genus level
alo_genus <- alo_spp_bayes %>% 
  mutate(genus = str_extract(species, "[:alpha:]+")) %>% 
  group_by(genus) %>% 
  summarize(a_g = mean(a, na.rm = T),
            b_g = mean(b, na.rm = T)) %>% 
  ungroup()

# Assemble final data ----------------------------------------------------------
length_weight <- clean_fish %>% 
  select(genus, species) %>% 
  distinct() %>% 
  left_join(alo_spp_bayes, by = "species") %>% 
  left_join(alo_genus, by = "genus") %>% 
  mutate(a = coalesce(a, a_g),
         b = coalesce(b, b_g)) %>% 
  select(species, a, b)

# EXPORT #######################################################################
write_csv(x = length_weight,
          file = here("data", "processed_data", "length_weight_parameters.csv"))
