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
  readxl,
  janitor,
  cowplot,
  tidyverse
)

# Source defaults --------------------------------------------------------------
source(here("scripts","_defaults.R"))

# Load data --------------------------------------------------------------------
# Lobster
argus <- read_csv(here("data", "raw_data", "lobster_tl_samples.csv"))
# Abalone
abalone <- read_excel(
  path = here("data", "raw_data", "transects", "COBI_Abulone_2006-2021_13jul2022.xlsx")) %>% 
  clean_names() %>% 
  filter(!comunidad == "Isla Magdalena") %>% 
  mutate(diametro = as.numeric(diametro) * 10) %>% 
  select(especie, diametro, abundancia) %>% 
  drop_na(diametro, abundancia) %>% 
  filter(diametro > 0) %>% 
  mutate(diametro_expanded = map2(diametro, abundancia, ~rep(.x, .y))) %>% 
  select(especie, diametro_expanded) %>% 
  unnest(diametro_expanded) %>% 
  rename(diametro = diametro_expanded)

## PROCESSING ##################################################################

# Number of organisms > minimum catch size -------------------------------------
# Lobster
n_lob <- dim(argus)[1]
n_above_lob <- argus %>% 
  mutate(above = carapace_length >= 74.6) %>% 
  summarize(n = sum(above) / length(above) * 100) %>% 
  pull(n) %>% 
  round(2) %>% 
  paste0("%")

# Abalone
n_abalone <- dim(abalone)[1]
n_above_abalone <- abalone %>% 
  mutate(above = diametro >= 136) %>% 
  summarize(n = sum(above) / length(above) * 100) %>% 
  pull(n) %>% 
  round(2) %>% 
  paste0("%")

# Build a histogram ------------------------------------------------------------
p1 <- ggplot(data = argus,
            mapping = aes(x = carapace_length)) +
  geom_histogram(color = "black",
                 fill = "cadetblue",
                 binwidth = 10) +
  geom_vline(xintercept = 74.6,
             linetype = "dashed") +
  geom_vline(aes(xintercept = mean(carapace_length)),
             color = "darkorange1") +
  theme_bw() +
  labs(x = "Carapace length (mm)",
       y = "Absolute frequency",
       subtitle = paste0(n_above_lob, " of sampled lobster (N = ", n_lob, ") are above the minimum catch size"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p2 <- ggplot(data = abalone,
       mapping = aes(x = diametro)) +
  geom_histogram(color = "black",
                 fill = "cadetblue",
                 binwidth = 20) +
  geom_vline(xintercept = 136,
             linetype = "dashed") +
  geom_vline(aes(xintercept = mean(diametro)),
             color = "darkorange1") +
  facet_wrap(~especie, scales = "free_y") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "italic"))+
  labs(x = "Shell diameter (mm)",
       y = "Absolute frequency",
       subtitle = paste0(n_above_abalone, "of sampled abalone (N =", n_abalone, ") are above the minimum catch size"))


p <- plot_grid(p1, p2, ncol = 1, labels = "auto")

## EXPORT ######################################################################
ggsave(plot = p,
       filename = here("results", "img", "fig_s1_invert_histogram.png"),
       width = 7, height = 9)  
