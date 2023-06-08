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
library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------
tot_val_ref <- readRDS(here("data", "output_data", "tot_val_ref.rds")) %>%
  select(community, group, ref_value_MXN_hect)
tot_val_ref2 <- readRDS(here("data", "output_data", "tot_val_ref_reviewer.rds")) %>%
  select(community, group, ref_value_MXN_hect2 = ref_value_MXN_hect)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
combined <- left_join(tot_val_ref,
                      tot_val_ref2,
                      by = c("community", "group"))

model <- lm(ref_value_MXN_hect2 ~ ref_value_MXN_hect, data = combined) %>% 
  broom::tidy()

fml <- paste0("y(x) = ",
              round(model$estimate[1], 2),
              " + ",
              round(model$estimate[2], 2),
              "x\n R^2 = 0.901\n F(1, 16) = 146.8\n p < 0.001")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
alternative_values <- ggplot(
  data = combined,
  mapping = aes(x = ref_value_MXN_hect,
                y = ref_value_MXN_hect2)) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(x = 25, y = 5, label = fml, inherit.aes = F, size = 3) +
  geom_point(mapping = aes(fill = community,
                           shape = group),
             color = "black",
             size = 3) +
  scale_fill_brewer(palette = "Set1") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Reference value based on historical minimum\n(MXP/ha)",
       y = "Reference value based on second smalles value\n(MXP/ha)",
       fill = "Community",
       shape = "Group") +
  guides(fill = guide_legend(ncol = 3,
                             override.aes = list(shape = 21)),
         shape = guide_legend(ncol = 2)) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1))
## EXPORT ######################################################################
ggsave(
  plot = alternative_values,
  filename = here("results", "img", "fig_s5_alternative_values.png"),
  width = 5.5,
  height = 5.5)
