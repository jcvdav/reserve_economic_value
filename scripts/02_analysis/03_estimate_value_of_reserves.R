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
  fixest,
  broom,
  tidyverse
)

# Load data --------------------------------------------------------------------
biomass_by_transect <- readRDS(file = here("data", "output_data", "summarized_biomass_value_by_transect.rds"))

## PROCESSING ##################################################################

# Summarize at the community level and get quantiles ---------------------------
biomass_by_community <- biomass_by_transect %>% 
  group_by(year, community, zone, group) %>%
  summarize(
    biomass_kg_hect_sd = sd(biomass_kg_hect),
    biomass_kg_hect = mean(biomass_kg_hect),
    value_MXN_hect_ci_low = quantile(value_MXN_hect, probs = 0.025),
    value_MXN_hect_ci_high = quantile(value_MXN_hect, probs = 0.975),
    sd_value_MXN_hect = sd(value_MXN_hect),
    value_MXN_hect = mean(value_MXN_hect),
    n_obs = n(),
    std.error = sd_value_MXN_hect / sqrt(n_obs - 1)
  ) %>%
  ungroup()

# Find historical minimums -----------------------------------------------------
tot_val_ref <- biomass_by_community %>% 
  group_by(community) %>% 
  mutate(year_min = min(year)) %>% 
  ungroup() %>% 
  filter((year < 2019 & zone == "Control") | (zone == "Reserve" & year == year_min)) %>% 
  filter(value_MXN_hect > 0) %>%
  group_by(community, group) %>% 
  mutate(val_min = min(value_MXN_hect)) %>% 
  ungroup() %>% 
  filter(value_MXN_hect == val_min) %>% 
  select(community, group, zone, year, ref_value_MXN_hect = value_MXN_hect) %>% 
  arrange(community, group) %>% 
  mutate(id2 = paste(community, group, zone, year, sep = "-"),
         type = "Reference")

tot_val_ref2 <- biomass_by_community %>% 
  group_by(community) %>% 
  mutate(year_min = min(year)) %>% 
  ungroup() %>% 
  filter((year < 2019 & zone == "Control") | (zone == "Reserve" & year == year_min)) %>% 
  filter(value_MXN_hect > 0) %>%
  group_by(community, group) %>% 
  filter(!value_MXN_hect == min(value_MXN_hect)) %>% 
  mutate(val_min = min(value_MXN_hect)) %>% 
  ungroup() %>% 
  filter(value_MXN_hect == val_min) %>% 
  select(community, group, zone, year, ref_value_MXN_hect = value_MXN_hect) %>% 
  arrange(community, group) %>% 
  mutate(id2 = paste(community, group, zone, year, sep = "-"),
         type = "Reference")

# Now calculate the extractive value -------------------------------------------
# First, find the reference points (combination of community, group, zone and year)
references <- tot_val_ref$id2

# Build estimation data --------------------------------------------------------
# Now filter the transect-level data to get present day and reference points only
estimation_data <- biomass_by_transect %>% 
  mutate(id2 = paste(community, group, zone, year, sep = "-")) %>% 
  filter((zone == "Reserve" & year == 2019) | id2 %in% references) %>% 
  select(-id2) %>% 
  mutate(today = 1 * (zone == "Reserve" & year == 2019),
         subgroup = paste(community, group))

# Regressions ------------------------------------------------------------------
total_value_models <- feols(fml = value_MXN_hect ~ 0 + today,
                           data = estimation_data,
                           split = ~subgroup,
                           vcov = "hetero")

extractive_value_models <- feols(fml = value_MXN_hect ~ today,
                                 data = estimation_data,
                                 split = ~subgroup,
                                 vcov = "hetero")

# Now build a data.frame -------------------------------------------------------
total_value <- map_dfr(total_value_models, tidy, .id = "model")
extractive_value <- map_dfr(extractive_value_models, tidy, .id = "model")

value_of_reserves <- bind_rows(total_value, extractive_value, .id = "type") %>% 
  mutate(
    type = ifelse(type == 1, "Total", "Extractive"),
    type = fct_relevel(type, c("Total", "Extractive")),
    model = str_remove(model, "sample.var: subgroup; sample: "),
    model = str_remove(model, "; rhs: 0 \\+ today|; rhs: today"),
    group = str_extract(model, "Finfish|Invertebrate"),
    community = str_remove(model, group),
    community = str_squish(community),
    term = case_when(type == "Total" ~ "Absolute",
                      type == "Extractive" & term == "today" ~ "Difference",
                      T ~ "Historical"),
    p.val.stars = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01 ~ "**",
      p.value <= 0.1 ~ "*",
      T ~ ""
    )) %>%
  select(community, group, type, value = term, estimate, std.error, statistic, p.value, p.val.stars) %>% 
  mutate(estimate = pmax(estimate, 0),
         std.error = (estimate > 0) * std.error) %>% 
  mutate(community = fct_relevel(community, com_order))

## EXPORT ######################################################################
# Model objects ----------------------------------------------------------------
saveRDS(obj = total_value_models,
        file = here("data", "output_data", "total_value_models.rds"))
saveRDS(obj = extractive_value_models,
        file = here("data", "output_data", "extractive_value_models.rds"))
# Table with coefficients ------------------------------------------------------
saveRDS(obj = value_of_reserves,
        file = here("data", "output_data", "value_of_reserves.rds"))
# Tot val ref ------------------------------------------------------------------
saveRDS(obj = tot_val_ref,
        here("data", "output_data", "tot_val_ref.rds"))
saveRDS(obj = tot_val_ref2,
        here("data", "output_data", "tot_val_ref_reviewer.rds"))