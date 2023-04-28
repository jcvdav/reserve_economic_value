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
  tidyverse
)

# Load data --------------------------------------------------------------------
# Transect data
clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv")) 
clean_inverts <- read_csv(file = here("data", "processed_data", "clean_invertebrate_transects.csv")) 


N_fish_transects <- clean_fish %>% 
  select(year, community, site, zone, transect, id) %>% 
  distinct() %>% 
  count(year,community) %>% 
  mutate(source = "Fish")


N_invert_transects <- clean_inverts %>% 
  select(year, community, site, zone, transect, id) %>% 
  distinct() %>% 
  count(year, community) %>% 
  mutate(source = "Invertebrates")


N_transects <- bind_rows(N_fish_transects,
                         N_invert_transects) %>% 
  mutate(community = fct_relevel(community, com_order))


transects_panel <- ggplot(data = N_transects,
                          aes(x = year, y = source, size = n, fill = source)) +
  geom_point(shape = 21) +
  scale_fill_manual(values = palette) +
  facet_wrap(~community) +
  labs(x = "Year",
       y = "",
       fill = "Survey type",
       size = "Number of transects") +
  scale_size_continuous(breaks = c(5, 50, 100, 170))

ggsave(
  plot = transects_panel,
  filename = here("results", "img", "fig_s4_transects_panel.png"),
  width = 8,
  height = 4)

  
