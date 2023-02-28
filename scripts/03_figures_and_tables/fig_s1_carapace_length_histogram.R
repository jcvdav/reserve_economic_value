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
argus <- read_csv(here("data", "raw_data", "lobster_tl_samples.csv"))

## PROCESSING ##################################################################

# Number of organisms > minimum catch size -------------------------------------
n_above <- argus %>% 
  mutate(above = carapace_length >= 74.6) %>% 
  summarize(n = sum(above) / length(above) * 100) %>% 
  pull(n) %>% 
  round(2) %>% 
  paste0("%")

# Build a histogram ------------------------------------------------------------
p <- ggplot(data = argus,
            mapping = aes(x = carapace_length)) +
  geom_histogram(color = "black",
                 fill = "cadetblue") +
  geom_vline(xintercept = 74.6, linetype = "dashed") +
  theme_bw() +
  labs(x = "Carapace length (mm)",
       y = "Absolute frequency",
       subtitle = paste(n_above, "of sampled lobster (N = 173) are above the minimum catch size"))

## EXPORT ######################################################################
ggsave(plot = p,
       filename = here("results", "img", "fig_s1_carapace_length_histogram.png"),
       width = 6, height = 4.5)  
