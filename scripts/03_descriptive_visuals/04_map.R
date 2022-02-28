######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

# Load packages
library(here)
library(ggrepel)
library(rnaturalearth)
library(tidyverse)

points <- tibble(community = c("Banco Chinchorro",
                               "Maria Elena",
                               "El Rosario",
                               "Puerto Libertad",
                               "Punta Herrero",
                               "Isla Natividad",
                               "La Bocana",
                               "Isla San Pedro Nolasco",
                               "Isla San Pedro Martir"),
                 lat = c(18.5877647, 19.407205, 29.947511,
                         29.9064768, 19.3129676, 27.8723402,
                         26.8031381,27.9692352, 28.3808945),
                 lon = c(-87.331836, -87.516371, -115.812187,
                         -112.6991437, -87.4545824, -115.2036612,
                         -113.7446934, -111.3873554, -112.3128677)) %>% 
  arrange(lon) %>% 
  mutate(id = 1:9)

mex <- ne_countries(country = "Mexico", scale = "medium", returnclass = "sf")
cont <- ne_countries(continent = "North America", returnclass = "sf", scale = "medium") %>% 
  filter(!iso_a3 == "MEX") 


map <- ggplot() +
  geom_sf(data = cont, color = "black", fill = "transparent") +
  geom_sf(data = mex, color = "black") +
  geom_point(data = points, aes(x = lon, y = lat)) +
  geom_text_repel(data = points, 
                  mappin = aes(x = lon, y = lat, label = id),
                  min.segment.length = 0,
                  nudge_x = c(-1, -1, -1, 1.5, 1.5, 1.5, 2, 1, 1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(-120, -85)) +
  scale_y_continuous(expand = c(0, 0), limits = c(12, 35)) +
  labs(x = "Longitude", y = "Latitude")

ggsave(plot = map, 
       filename = here("results", "img", "map.png"),
       width = 6, height = 4)
