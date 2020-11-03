


# Plan:
#   self isolation against self_report 7 and 14 maxa
#   self isolation duration and self_report 7 and 14 max
#   sensitivity against self_isolation



library(data.table)
library(tidyverse)
library(git2r)
library(tictoc)
library(ggplot2)
library(patchwork)
library(cowplot)
library(latex2exp)
library(furrr)
library(sn)
library(ggrepel)
library(testthat)
library(patchwork)
library(binom)
devtools::load_all()

# git2r::revparse_single('.',"HEAD")$sha

set.seed(20000529)

#' Delay shape is adherence probability
#'
#' Cap cases was chosen in a seperate analysis (choose_cap.R or something.)
no.samples <- 20000

# Scenario 1: 90% self reporting and contact reporting, 60% isolation  adherence

# alter delay shape (which actually controls reporting) and iso_adhere and max_isolation 
scenarios1 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.2, 0.3),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 2.115779,
  inf_rate = 0.6898583,
  inf_shift = 3,
  min_quar_delay = 1,
  max_quar_delay = 1,
  index_R0 = c(1.3),
  prop.asym = c(0.5),
  asymptomatic_transmission = 0.5,
  min_isolation = c(7, 14),
  max_isolation = c(7, 14),
  control_effectiveness = seq(0.4, 0.8, 0.1),
  self_report = 1,
  iso_adhere = c(0.2, 0.3),
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(20)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n()) %>% 
  filter(min_isolation == max_isolation)

scenarios1 %>% dim

cap_cases <- 1500
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

future::plan("multicore", workers = 8)

#+ full_run
tic()
## Run parameter sweep
sweep_results1 <- ringbp::parameter_sweep(scenarios1,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut = FALSE)
toc()



saveRDS(sweep_results1, file = "data-raw/res_seven_day.rds")

#if(!exists('sweep_results1'))  sweep_results1 <- readRDS(file = "data-raw/res_seven_day.rds")



sweep_results1 <- 
  sweep_results1 %>% 
  mutate(pext = sims) %>% 
  mutate(lower = binom.confint(pext * no.samples, no.samples, method = 'exact')$lower) %>% 
  mutate(upper = binom.confint(pext * no.samples, no.samples, method = 'exact')$upper) 
  



sweep_results1 %>% 
  filter(control_effectiveness != 0) %>% 
  filter(delay_shape <= 0.3, iso_adhere <= 0.3) %>% 
  filter(!(delay_shape == 0.3 & max_isolation == 14)) %>% 
  filter(!(iso_adhere == 0.3 & max_isolation == 14)) %>% 
  mutate(adherence = paste0('sr', delay_shape, 'i', iso_adhere)) %>% 
  mutate(adherence = factor(adherence, 
                            labels = c('SR: 20%, Iso: 20%',
                                      'SR: 20%, Iso: 30%',
                                      'SR: 30%, Iso: 20%',
                                      'SR: 30%, Iso: 30%'))) %>% 
  ggplot(aes(control_effectiveness, y = 1 - pext, 
             colour = factor(adherence),
             linetype = factor(max_isolation))) + 
    geom_line() +
    geom_errorbar(aes(ymax = 1 - upper, ymin = 1 - lower), width = 0) +
    labs(linetype = 'Requested iso.',
         colour = 'Adherence') +
    ylab('Risk of large outbreak') +
    ggtitle('Benefits of 7/14 days and adherence')

ggsave('inst/plots/seven_days_vs_20pc_adherence.png')

