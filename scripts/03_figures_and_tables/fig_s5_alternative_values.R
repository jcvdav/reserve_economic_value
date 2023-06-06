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

# Load data --------------------------------------------------------------------
tot_val_ref <- readRDS(here("data", "output_data", "tot_val_ref.rds")) %>%
  select(community, group, ref_value_MXN_hect)
tot_val_ref2 <- readRDS(here("data", "output_data", "tot_val_ref_reviewer.rds")) %>%
  select(community, group, ref_value_MXN_hect2 = ref_value_MXN_hect)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
combined <- left_join(a, b, by = c("community", "group"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
alternative_values <- ggplot(
  data = combined,
  mapping = aes(x = ref_value_MXN_hect,
                y = ref_value_MXN_hect2,
                color = community,
                shape = group)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Reference value based on historical minimum\n(MXP/ha)",
       y = "Reference value based on second smalles value\n(MXP/ha)",
       color = "Community",
       shape = "Group") +
  guides(color = guide_legend(ncol = 3),
         shape = guide_legend(ncol = 2)) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1))
## EXPORT ######################################################################
ggsave(
  plot = alternative_values,
  filename = here("results", "img", "fig_s5_alternative_values.png"),
  width = 5,
  height = 5)
