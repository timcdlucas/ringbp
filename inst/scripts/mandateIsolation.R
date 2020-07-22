


# Plan:
#   self isolation against self_report with three different tradeoffs
#   self isolation against self_report AND contact report with three tradeoffs
#   self isolation duration and self isolation against self_report with three different tradeoffs



# self isolation against self_report with three different tradeoffs
#   If you mandate self_isolation, fewer people will self report


# self isolation against self_report AND contact report with three tradeoffs
#  If you mandate self_isolation, self reporting AND contact reporting will be lower.


# self isolation duration and self isolation against self_report with three different tradeoffs
#   If you don't mandate self isolation, self reporting will be higher. But maybe most people will self_isolate for a week if not two weeks.







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

devtools::load_all()

# git2r::revparse_single('.',"HEAD")$sha

set.seed(200529)

#' Delay shape is adherence probability
#'
#' Cap cases was chosen in a seperate analysis (choose_cap.R or something.)
no.samples <- 10

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
  index_R0 = c(1.1),
  prop.asym = c(0.5),
  min_isolation = 14,
  max_isolation = c(7, 14),
  control_effectiveness = c(0, seq(0.4, 0.8, 0.1)),
  self_report = 1,
  iso_adhere = c(0.3, 0.5, 0.7, 0.9),
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(10)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

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

if(!exists('sweept_results1')){
  sweep_results1 <- readRDS(file = "data-raw/res_adhere.rds")
}

sweep_results1 <- 
  sweep_results1 %>% 
  mutate(pext = sims) 


sweep_results1 %>% 
  filter(control_effectiveness %in% c(0.4, 0.5, 0.6, 0.8)) %>%
  filter(max_isolation == 14) %>% 
  mutate(control_effectiveness = 
           factor(ifelse(control_effectiveness == "0.8", 'control cov. = 0.8', control_effectiveness),
         levels = c('control cov. = 0.8', '0.6', '0.5', '0.4'))) %>%  
  ggplot(aes(delay_shape, iso_adhere, fill = 1 - pext)) + 
  geom_tile() +
  facet_grid(control_effectiveness ~ max_isolation) +
  scale_fill_viridis_c() +
  labs(fill = 'Risk') +
  xlab('Self report') + 
  ylab('Isolation adherence') +
  theme(text = element_text(size = 20))
ggsave('inst/plots/heatmap_adhere.pdf')


sweep_results1 %>% 
  filter(self_report %in% c(0.4, 0.5, "0.6", 0.8), iso_adhere %in% c(0.4, 0.5, "0.6", 0.8)) %>% 
  mutate(self_report = factor(ifelse(self_report == 0.8, 'self rep=0.8', self_report), 
                              levels = c('self rep=0.8', "0.6", "0.5", "0.4"))) %>% 
  mutate(iso_adhere = factor(ifelse(iso_adhere == 0.4, 'isolate=0.4', iso_adhere), 
                             levels = c('isolate=0.4', "0.5", "0.6", "0.8"))) %>% 
  filter(index_R0 == 1.1) %>%
  ggplot(aes(control_effectiveness, y = 1 - pext)) + 
  geom_line() +
  facet_grid(self_report ~ iso_adhere) +
  ylab('Risk') +
  xlab('Control effectiveness') +
  scale_x_continuous(breaks = c(0.5, 0.7)) +
  ggtitle('Rs = 1.1')+
  theme(text = element_text(size = 20))
ggsave('inst/plots/ready_reckoner_adhere.pdf')



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
  index_R0 = c(1.1),
  prop.asym = c(0.5),
  min_isolation = c(1, 4, 7, 14),
  max_isolation = c(7, 14),
  control_effectiveness = c(0, seq(0.4, 0.8, 0.1)),
  self_report = 1,
  iso_adhere = 0.7,
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(10)) %>%
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


if(!exists('sweept_results2')){
  sweep_results2 <- readRDS(file = "data-raw/res_duration.rds")
}


sweep_results2 <- 
  sweep_results2 %>% 
    mutate(pext = sims) 



sweep_results2 %>% 
  filter(control_effectiveness %in% c(0.4, 0.5, 0.6, 0.8)) %>%
  filter(max_isolation == 14) %>% 
  mutate(control_effectiveness = 
           factor(ifelse(control_effectiveness == "0.8", 'control cov. = 0.8', control_effectiveness),
                  levels = c('control cov. = 0.8', '0.6', '0.5', '0.4'))) %>%  
  ggplot(aes(delay_shape, min_isolation, fill = 1 - pext)) + 
  geom_tile() +
  facet_grid(control_effectiveness ~ max_isolation) +
  scale_fill_viridis_c() +
  labs(fill = 'Risk') +
  xlab('Self report') + 
  ylab('Isolation duration') +
  theme(text = element_text(size = 20))
ggsave('inst/plots/heatmap_duration.pdf')



sweep_results2 %>% 
  filter(self_report %in% c(0.4, 0.5, "0.6", 0.8), iso_adhere %in% c(0.4, 0.5, "0.6", 0.8)) %>% 
  mutate(self_report = factor(ifelse(self_report == 0.8, 'self rep=0.8', self_report), 
                              levels = c('self rep=0.8', "0.6", "0.5", "0.4"))) %>% 
  mutate(iso_adhere = factor(ifelse(iso_adhere == 0.4, 'isolate=0.4', iso_adhere), 
                             levels = c('isolate=0.4', "0.5", "0.6", "0.8"))) %>% 
  ggplot(aes(control_effectivenessraw, colour = factor(index_R0), y = 1 - pext)) + 
  geom_line() +
  labs(colour = 'Rs') +
  scale_x_continuous(breaks = c(0.5, 0.7)) +
  ylab('Risk') +
  xlab('Control effectiveness') +
  facet_grid(self_report ~ iso_adhere) +
  theme(text = element_text(size = 20))

ggsave('inst/plots/ready_reckoner_duration.pdf')






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
  index_R0 = c(1.1),
  prop.asym = c(0.5),
  min_isolation = 14,
  max_isolation = c(7, 14),
  control_effectiveness = c(0, seq(0.4, 0.8, 0.1)),
  self_report = 1,
  iso_adhere = c(0.3, 0.5, 0.7, 0.9),
  test_delay = c(1), #time from isolation to test result
  sensitivity = c(0.35, 0.45, 0.55, 0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(10)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

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


if(!exists('sweept_results3')){
  sweep_results3 <- readRDS(file = "data-raw/res_sensitivity.rds")
}


sweep_results3 <- 
  sweep_results3 %>% 
  mutate(pext = sims) 




sweep_results3 %>% 
  filter(control_effectiveness %in% c(0.4, 0.5, 0.6, 0.8)) %>%
  filter(max_isolation == 14) %>% 
  mutate(control_effectiveness = 
           factor(ifelse(control_effectiveness == "0.8", 'control cov. = 0.8', control_effectiveness),
                  levels = c('control cov. = 0.8', '0.6', '0.5', '0.4'))) %>%  
  ggplot(aes(sensitivity, iso_adhere, fill = 1 - pext)) + 
  geom_tile() +
  facet_grid(control_effectiveness ~ max_isolation) +
  scale_fill_viridis_c() +
  labs(fill = 'Risk') +
  xlab('Self report') + 
  ylab('Isolation adherence') +
  theme(text = element_text(size = 20))
ggsave('inst/plots/heatmap_sensitivity.pdf')




sweep_results3 %>% 
  filter(index_R0 == 1.1) %>%
  filter(sensitivity %in% c(0.35, 0.45, 0.55, 0.65), iso_adhere %in% c(0.4, 0.5, "0.6", 0.8)) %>% 
  mutate(sensitivity = factor(ifelse(sensitivity == 0.65, 'sensitivity=0.65', sensitivity), 
                              levels = c('sensitivity=0.65', "0.55", "0.45", "0.35"))) %>% 
  mutate(iso_adhere = factor(ifelse(iso_adhere == 0.4, 'isolate=0.4', iso_adhere), 
                             levels = c('isolate=0.4', "0.5", "0.6", "0.8"))) %>% 
  ggplot(aes(control_effectiveness, y = 1 - pext)) + 
  geom_line() +
  labs(colour = 'Rs') +
  scale_x_continuous(breaks = c(0.5, 0.7)) +
  ylab('Risk') +
  xlab('Control effectiveness') +
  ggtitle('Rs = 1.1. Sensitivity vs isolation adherence')+
  facet_grid(sensitivity ~ iso_adhere) +
  theme(text = element_text(size = 20))

ggsave('inst/plots/ready_reckoner_sensitivity.pdf')




