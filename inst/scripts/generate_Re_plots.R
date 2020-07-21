
# Packages ----------------------------------------------------------------

library(ringbp)
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)
library(tidyr)

# Load in results  -------------------------------------------------------

sweep_results <- readRDS("../../data-raw/res_20200518_complete.rds")

d <- sweep_results %>% 
       filter(self_report == 0.5,
              max_quar_delay == 1,
              sensitivity == 0.65,
              test_delay == 2) %>% 
  dplyr::mutate(effective_r0 = purrr::map(
    sims,
    ~ dplyr::group_by(., sim) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(median_eff_r0 = median(effective_r0,
                                              na.rm = TRUE),
                       lower = quantile(effective_r0, 0.025,
                                        na.rm = TRUE),
                       iqr_lower = quantile(effective_r0,
                                            0.25,
                                            na.rm = TRUE),
                       iqr_upper = quantile(effective_r0,
                                            0.75,
                                            na.rm = TRUE),
                       upper = quantile(effective_r0,
                                        0.975,
                                        na.rm = TRUE))
  )) %>%
  tidyr::unnest("effective_r0")



d %>% 
  ggplot(aes(x = control_effectiveness, colour = factor(index_R0), y = median_eff_r0)) + 
    facet_wrap(~ precaution) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(x = control_effectiveness, ymin = lower, ymax = upper, fill = factor(index_R0)), 
                inherit.aes = FALSE, alpha = 0.2)



d %>% 
  ggplot(aes(x = control_effectiveness, y = median_eff_r0)) + 
  facet_wrap(factor(precaution) ~ factor(index_R0)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(x = control_effectiveness, ymin = lower, ymax = upper), 
              inherit.aes = FALSE, alpha = 0.2)

ggsave('../plots/effective_R0_precaution_index_facet.pdf')

d %>% 
  ggplot(aes(x = control_effectiveness, y = median_eff_r0)) + 
  facet_wrap(factor(precaution) ~ factor(index_R0)) +
  geom_line() +
  geom_point() 
ggsave('../plots/effective_R0_precaution_index_facet_noci.pdf')

