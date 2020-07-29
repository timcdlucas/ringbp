


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

set.seed(200529)

#' Delay shape is adherence probability
#'
#' Cap cases was chosen in a seperate analysis (choose_cap.R or something.)
no.samples <- 15000

# Scenario 1: 90% self reporting and contact reporting, 60% isolation  adherence

# alter delay shape (which actually controls reporting) and iso_adhere and max_isolation 
scenarios1 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.3, 0.5, 0.7, 0.9),
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
  control_effectiveness = c(0, seq(0.4, 0.8, 0.1)),
  self_report = 1,
  iso_adhere = c(0.3, 0.5, 0.7, 0.9),
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(20)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n()) %>% 
  filter(min_isolation == max_isolation)

scenarios1 %>% dim

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results1 <- ringbp::parameter_sweep(scenarios1,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut = FALSE)
toc()



saveRDS(sweep_results1, file = "data-raw/res_adhere.rds")

#if(!exists('sweep_results1'))  sweep_results1 <- readRDS(file = "data-raw/res_adhere.rds")


sweep_results1 <- 
  sweep_results1 %>% 
  mutate(pext = sims) %>% 
  mutate(lower = binom.confint(pext * no.samples, no.samples, method = 'exact')$lower) %>% 
  mutate(upper = binom.confint(pext * no.samples, no.samples, method = 'exact')$upper) 
  
sweep_results1 %>% 
  filter(control_effectiveness %in% c(0.4, 0.5, "0.6", 0.8)) %>%
  mutate(control_effectiveness = 
           factor(ifelse(control_effectiveness == "0.8", 'Control Eff. = 0.8', control_effectiveness),
         levels = c('Control Eff. = 0.8', '0.6', '0.5', '0.4'))) %>%  
  ggplot(aes(delay_shape, iso_adhere, fill = 1 - pext)) + 
  geom_tile() +
  facet_grid(control_effectiveness ~ max_isolation) +
  scale_fill_viridis_c() +
  labs(fill = 'Risk') +
  xlab('Self report') + 
  ylab('Isolation adherence') +
  theme(text = element_text(size = 20))
ggsave('inst/plots/heatmap_adhere.pdf', width = 6, height = 9)


sweep_results1 %>% 
  filter(control_effectiveness != 0) %>% 
  mutate(delay_shape = factor(ifelse(delay_shape == 0.9, 'self rep=90%', paste0(100*delay_shape, '%')), 
                              levels = c('self rep=90%', "70%", "50%", "30%"))) %>% 
  mutate(iso_adhere = factor(ifelse(iso_adhere == 0.3, 'isolate=30%', paste0(100*iso_adhere, '%')), 
                             levels = c('isolate=30%', "50%", "70%", "90%"))) %>% 
  ggplot(aes(control_effectiveness, y = 1 - pext, colour = factor(max_isolation))) + 
    geom_line() +
    geom_errorbar(aes(ymax = 1 - upper, ymin = 1 - lower), width = 0) +
    facet_grid(delay_shape ~ iso_adhere) +
    ylab('Risk of large outbreak') +
    xlab('Control effectiveness') +
    scale_x_continuous(breaks = c(0.5, 0.7), labels = c('50%', '70%')) +
    scale_y_continuous(breaks = c(0, 0.02, 0.04, 0.06), labels = c('0%', '2%', '4%', '6%')) +
    #ggtitle('Isolation adherence (probability)')+
    theme(text = element_text(size = 20)) +
    labs(colour = 'Isolation Duration (days)') +
    theme(legend.position="bottom")
ggsave('inst/plots/ready_reckoner_adhere.pdf', height = 8, width = 8)



sweep_results1 %>% 
  mutate(delay_shape = factor(ifelse(delay_shape == 0.9, 'self rep=0.9', delay_shape), 
                              levels = c('self rep=0.9', "0.7", "0.5", "0.3"))) %>% 
  mutate(iso_adhere = factor(ifelse(iso_adhere == 0.3, 'isolate=0.3', iso_adhere), 
                             levels = c('isolate=0.3', "0.5", "0.7", "0.9"))) %>% 
  ggplot(aes(control_effectiveness, y = 1 - pext, colour = factor(max_isolation))) + 
  geom_line() +
  facet_grid(delay_shape ~ iso_adhere) +
  ylab('Risk') +
  xlab('Control effectiveness') +
  scale_x_continuous(breaks = c(0.5, 0.7)) +
  ggtitle('Isolation adherence (probability)') +
  theme(text = element_text(size = 20)) +
  labs(colour = 'Max iso')
ggsave('inst/plots/ready_reckoner_adhere0.pdf', height = 7, width = 9)


rm(sweep_results1)

###########################################################################################

# Alter delay_shape (self reporting) and min isolation. As well as max isolation.
scenarios2 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.3, 0.5, 0.7, 0.9),
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
  min_isolation = c(1, 4, 7, 14),
  max_isolation = c(7, 14),
  control_effectiveness = c(0, seq(0.4, 0.8, 0.1)),
  self_report = 1,
  iso_adhere = 0.7,
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(20)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n()) %>% 
  filter(max_isolation >= min_isolation)

dim(scenarios2)

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results2 <- ringbp::parameter_sweep(scenarios2,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = FALSE,
                                          earlyOut = FALSE)
toc()



saveRDS(sweep_results2, file = "data-raw/res_duration.rds")


#if(!exists('sweep_results2'))  sweep_results2 <- readRDS(file = "data-raw/res_duration.rds")



sweep_results2 <- 
  sweep_results2 %>% 
  mutate(pext = sims) %>% 
  mutate(lower = binom.confint(pext * no.samples, no.samples, method = 'exact')$lower) %>% 
  mutate(upper = binom.confint(pext * no.samples, no.samples, method = 'exact')$upper) 



sweep_results2 %>% 
  filter(control_effectiveness %in% c("0.4", 0.5, "0.6", 0.8)) %>%
  mutate(control_effectiveness = 
           factor(ifelse(control_effectiveness == "0.8", 'Control Eff. = 0.8', control_effectiveness),
                  levels = c('Control Eff. = 0.8', '0.6', '0.5', '0.4'))) %>%  
  ggplot(aes(delay_shape, min_isolation, fill = 1 - pext)) + 
  geom_tile() +
  facet_grid(control_effectiveness ~ max_isolation) +
  scale_fill_viridis_c() +
  labs(fill = 'Risk') +
  xlab('Self report') + 
  ylab('Isolation adherence') +
  theme(text = element_text(size = 20))
ggsave('inst/plots/heatmap_duration.pdf', width = 6, height = 9)



sweep_results2 %>% 
  filter(control_effectiveness != 0) %>% 
  mutate(delay_shape = factor(ifelse(delay_shape == 0.9, 'self rep=90%', paste0(100*delay_shape, '%')), 
                              levels = c('self rep=90%', "70%", "50%", "30%"))) %>% 
  mutate(min_isolation = factor(ifelse(min_isolation == 1, 'min isolation=1', min_isolation), 
                             levels = c('min isolation=1', "4", "7", "14"))) %>% 
  ggplot(aes(control_effectiveness, y = 1 - pext, colour = factor(max_isolation))) + 
  geom_line() +
  geom_line() +
  geom_errorbar(aes(ymax = 1 - upper, ymin = 1 - lower), width = 0) +
  facet_grid(delay_shape ~ min_isolation) +
  ylab('Risk of large outbreak') +
  xlab('Control effectiveness') +
  scale_x_continuous(breaks = c(0.5, 0.7), labels = c('50%', '70%')) +
  scale_y_continuous(breaks = c(0, 0.02, 0.04), labels = c('0%', '2%', '4%')) +
  #ggtitle('Isolation adherence (probability)')+
  theme(text = element_text(size = 20)) +
  labs(colour = 'Max Isolation Duration (days)') +
  theme(legend.position="bottom")
ggsave('inst/plots/ready_reckoner_duration.pdf', height = 8, width = 8)



sweep_results2 %>% 
  mutate(delay_shape = factor(ifelse(delay_shape == 0.9, 'self rep=0.9', delay_shape), 
                              levels = c('self rep=0.9', "0.7", "0.5", "0.3"))) %>% 
  mutate(min_isolation = factor(ifelse(min_isolation == 1, 'min isolation=1', min_isolation), 
                                levels = c('min isolation=1', "4", "7", "14"))) %>% 
  ggplot(aes(control_effectiveness, y = 1 - pext, colour = factor(max_isolation))) + 
  geom_line() +
  facet_grid(delay_shape ~ min_isolation) +
  ylab('Risk') +
  xlab('Control effectiveness') +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7)) +
  ggtitle('Isolation adherence (duration)')+
  theme(text = element_text(size = 20)) +
  labs(colour = 'Max iso')
ggsave('inst/plots/ready_reckoner_duration0.pdf', height = 7, width = 9)


rm(sweep_results2)

################################################################################################

# vary sensitivity, iso adhere and max isolation.
scenarios3 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = 0.7,
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
  control_effectiveness = c(0, seq(0.4, 0.8, 0.1)),
  self_report = 1,
  iso_adhere = c(0.3, 0.5, 0.7, 0.9),
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.35, 0.45, 0.55, 0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(20)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n()) %>% 
  filter(max_isolation >= min_isolation)

scenarios3 %>% dim

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

future::plan("multicore")

#+ full_run
tic()
## Run parameter sweep
sweep_results3 <- ringbp::parameter_sweep(scenarios3,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = FALSE,
                                          earlyOut = FALSE)
toc()


saveRDS(sweep_results3, file = "data-raw/res_sensitivity.rds")


#if(!exists('sweep_results3'))  sweep_results3 <- readRDS(file = "data-raw/res_sensitivity.rds")



sweep_results3 <- 
  sweep_results3 %>% 
  mutate(pext = sims) %>% 
  mutate(lower = binom.confint(pext * no.samples, no.samples, method = 'exact')$lower) %>% 
  mutate(upper = binom.confint(pext * no.samples, no.samples, method = 'exact')$upper) 




sweep_results3 %>% 
  filter(control_effectiveness %in% c(0.4, 0.5, "0.6", 0.8)) %>%
  mutate(control_effectiveness = 
           factor(ifelse(control_effectiveness == "0.8", 'Control Eff. = 0.8', control_effectiveness),
                  levels = c('Control Eff. = 0.8', '0.6', '0.5', '0.4'))) %>%  
  ggplot(aes(sensitivity, iso_adhere, fill = 1 - pext)) + 
  geom_tile() +
  facet_grid(control_effectiveness ~ max_isolation) +
  scale_fill_viridis_c() +
  labs(fill = 'Risk') +
  xlab('Sensitivity') + 
  ylab('Isolation adherence') +
  theme(text = element_text(size = 20))
ggsave('inst/plots/heatmap_sensitivity.pdf', width = 6, height = 9)





sweep_results2 %>% 
  filter(control_effectiveness != 0) %>% 
  mutate(delay_shape = factor(ifelse(delay_shape == 0.9, 'self rep=90%', paste0(100*delay_shape, '%')), 
                              levels = c('self rep=90%', "70%", "50%", "30%"))) %>% 
  mutate(min_isolation = factor(ifelse(min_isolation == 1, 'min isolation=1', min_isolation), 
                                levels = c('min isolation=1', "4", "7", "14"))) %>% 
  ggplot(aes(control_effectiveness, y = 1 - pext, colour = factor(max_isolation))) + 
  geom_line() +
  geom_errorbar(aes(ymax = 1 - upper, ymin = 1 - lower), width = 0) +
  facet_grid(delay_shape ~ min_isolation) +
  ylab('Risk of large outbreak') +
  xlab('Control effectiveness') +
  scale_x_continuous(breaks = c(0.5, 0.7), labels = c('50%', '70%')) +
  scale_y_continuous(breaks = c(0, 0.02, 0.04), labels = c('0%', '2%', '4%')) +
  #ggtitle('Isolation adherence (probability)')+
  theme(text = element_text(size = 20)) +
  labs(colour = 'Max Isolation Duration (days)') +
  theme(legend.position="bottom")


sweep_results3 %>% 
  filter(min_isolation == max_isolation) %>% 
  filter(control_effectiveness != 0) %>% 
  mutate(iso_adhere = factor(ifelse(iso_adhere == 0.3, 'isolate=30%', paste0(100*iso_adhere, '%')), 
                             levels = c('isolate=30%', "50%", "70%", "90%"))) %>% 
  mutate(sensitivity = factor(ifelse(sensitivity == 0.65, 'sensitiv=65%', paste0(100*sensitivity, '%')), 
                              levels = c('sensitiv=65%', "55%", "45%", "35%"))) %>% 
  ggplot(aes(control_effectiveness, y = 1 - pext, colour = factor(max_isolation))) + 
  geom_line() +
  geom_errorbar(aes(ymax = 1 - upper, ymin = 1 - lower), width = 0) +
  facet_grid(sensitivity ~ iso_adhere) +
  ylab('Risk of large outbreak') +
  xlab('Control effectiveness') +
  scale_x_continuous(breaks = c(0.5, 0.7), labels = c('50%', '70%')) +
  scale_y_continuous(breaks = c(0, 0.02, 0.04, 0.06), labels = c('0%', '2%', '4%', '6%')) +
  #  ggtitle('Sensitivity')+
  theme(text = element_text(size = 20)) +
  labs(colour = 'Isolation Duration (days)') +
  theme(legend.position="bottom")
ggsave('inst/plots/ready_reckoner_sensitivity.pdf', height = 8, width = 8)

  

sweep_results3 %>% 
  filter(min_isolation == max_isolation) %>% 
  mutate(iso_adhere = factor(ifelse(iso_adhere == 0.3, 'isolate=0.3', iso_adhere), 
                             levels = c('isolate=0.3', "0.5", "0.7", "0.9"))) %>% 
  mutate(sensitivity = factor(ifelse(sensitivity == 0.65, 'sens. = 0.65', sensitivity), 
                              levels = c('sens. = 0.65', "0.55", "0.45", "0.35"))) %>% 
  ggplot(aes(control_effectiveness, y = 1 - pext, colour = factor(max_isolation))) + 
  geom_line() +
  facet_grid(sensitivity ~ iso_adhere) +
  ylab('Risk') +
  xlab('Control effectiveness') +
  scale_x_continuous(breaks = c(0.5, 0.7)) +
  ggtitle('Sensitivity')+
  theme(text = element_text(size = 20)) +
  labs(colour = 'Max iso')
ggsave('inst/plots/ready_reckoner_sensitivity0.pdf', height = 7, width = 9)




