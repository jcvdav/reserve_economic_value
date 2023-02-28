######################################################
#title#
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
  ggrepel,
  rnaturalearth,
  tidyverse
)

# Source defaults --------------------------------------------------------------
source(here("scripts","_defaults.R"))

# Load data --------------------------------------------------------------------
# Site coordinates
points <- tibble(community = c("Banco Chinchorro",
                               "Maria Elena",
                               "El Rosario",
                               "Puerto Libertad",
                               "Punta Herrero",
                               "Isla Natividad",
                               "La Bocana",
                               "Isla San Pedro Nolasco",
                               "Isla San Pedro Mártir"),
                 lat = c(18.5877647, 19.407205, 29.947511,
                         29.9064768, 19.3129676, 27.8723402,
                         26.8031381,27.9692352, 28.3808945),
                 lon = c(-87.331836, -87.516371, -115.812187,
                         -112.6991437, -87.4545824, -115.2036612,
                         -113.7446934, -111.3873554, -112.3128677)) %>% 
  arrange(lon) %>% 
  mutate(id = 1:9) %>% 
  mutate(community = fct_relevel(community, com_order))

# Mexico's coastline
mex <- ne_countries(country = "Mexico",
                    scale = "medium",
                    returnclass = "sf")

# Continent coastine
cont <- ne_countries(continent = "North America",
                     scale = "medium",
                     returnclass = "sf") %>% 
  filter(!iso_a3 == "MEX")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
map <- ggplot() +
  geom_sf(data = cont, 
          color = "black",
          fill = "gray90",
          linewidth = 0.5) +
  geom_sf(data = mex,
          color = "black",
          fill = "gray80",
          linewidth = 0.5) +
  geom_point(data = points, 
             aes(x = lon, y = lat, fill = community),
             shape = 21,
             color = "black",
             size = 2) +
  geom_text_repel(data = points, 
                  mapping = aes(x = lon, y = lat, label = id),
                  min.segment.length = 0,
                  nudge_x = c(-1.5, -1.5, -1.5, 1.5, 1.5, 1.5, 2, 2, 2)) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-120, -85)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(12, 35)) +
  theme(legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_rect(fill = "white",
                                         colour = "black",
                                         linewidth = 0.1)) +
  guides(fill = guide_legend(title = "Community",
                             ncol = 2)) +
  labs(x = "Longitude",
       y = "Latitude")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

# Load packages

ggsave(plot = map, 
       filename = here("results", "img", "fig_1_map.png"),
       width = 8, height = 5.3)
