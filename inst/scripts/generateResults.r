# Running the model and storing outputs

library(data.table)
library(tidyverse)
library(git2r)
library(tictoc)
library(ggplot2)
library(patchwork)
library(cowplot)
library(furrr)
library(sn)
library(ggrepel)
library(testthat)
library(svglite)

#########################
# General results (Figs 1b and 3c)

rm(list = ls())
devtools::load_all()

set.seed(200518)

no.samples <- 5000

tic()
scenarios1 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.9),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = c(0,seq(0.4, 1, 0.2)),
  self_report = c(0.5),
  test_delay = c(0,2), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

#+ full_run
tic()
## Run parameter sweep
sweep_results1 <- ringbp::parameter_sweep(scenarios1,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE)
toc()


# #+ writeout
saveRDS(sweep_results1, file = "data-raw/res_Aug_1.rds")
rm(list=ls())
Sys.sleep(120)
# tic()
# sweep_results1 <- readRDS("data-raw/res_Aug_1.rds")
# toc()
##################################################################


set.seed(200518)

#' Delay shape is adherence probability
#'
#' Cap cases was chosen in a seperate analysis (choose_cap.R or something.)
no.samples <- 5000

scenarios2 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.9),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = c(0,seq(0.4, 1, 0.2)),
  self_report = c(0.5),
  test_delay = c(0,2), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

#+ full_run
tic()
## Run parameter sweep
sweep_results2 <- ringbp::parameter_sweep(scenarios2,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE)
toc()


# #+ writeout
saveRDS(sweep_results2, file = "data-raw/res_Aug_2.rds")
rm(list=ls())
Sys.sleep(120)


set.seed(200518)
no.samples <- 5000

scenarios3 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.9),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.3),
  prop.asym = c(0.4),
  control_effectiveness = c(0,seq(0.4, 1, 0.2)),
  self_report = c(0.5),
  test_delay = c(0,2), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(0,7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

tic()
## Run parameter sweep
sweep_results3 <- ringbp::parameter_sweep(scenarios3,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE)

toc()

# #+ writeout
saveRDS(sweep_results3, file = "data-raw/res_Aug_3.rds")
rm(list=ls())
Sys.sleep(120)
##################################################################

set.seed(200518)
no.samples <- 5000

scenarios4 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.9),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(4),
  index_R0 = c(1.3),
  prop.asym = c(0.4),
  control_effectiveness = c(0,seq(0.4, 1, 0.2)),
  self_report = c(0.5),
  test_delay = c(2), #time from isolation to test result
  sensitivity = c(0), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

#+ full_run
tic()
## Run parameter sweep
sweep_results4 <- ringbp::parameter_sweep(scenarios4,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE)

toc()


# #+ writeout
saveRDS(sweep_results4, file = "data-raw/res_Aug_4.rds")
rm(list=ls())
##################################################################

toc()

# Load in separate 4 sets of scenarios and combine
sweep_results1 <- readRDS("data-raw/res_Aug_1.rds")
sweep_results2 <- readRDS("data-raw/res_Aug_2.rds")
sweep_results3 <- readRDS("data-raw/res_Aug_3.rds")
sweep_results4 <- readRDS("data-raw/res_Aug_4.rds")
#
sweep_results <- rbind(sweep_results1,sweep_results2,sweep_results3,sweep_results4)

# Clear redundant data frames from working memory
sweep_results1 = sweep_results2 = sweep_results3 = sweep_results4 = c()

# Expand results for no testing so they can be filtered for plotting later on
# Same results hold for precaution =0,7 and test delay =0,2
temp1 <- sweep_results %>% filter(sensitivity==0) %>%
  mutate(precaution := 0)
temp2 <- sweep_results %>% filter(sensitivity==0) %>%
  mutate(precaution := 0) %>%
  mutate(test_delay := 0)
temp3 <- sweep_results %>% filter(sensitivity==0) %>%
  mutate(test_delay := 0)
temp <- rbind(temp1,temp2,temp3)
sweep_results <- rbind(sweep_results,temp)
temp = temp1 = temp2 = temp3 = c()
temp <- sweep_results %>% filter(sensitivity==0) %>%
  mutate(max_quar_delay := 1)
sweep_results <- rbind(sweep_results,temp)
sweep_results$scenario <- 1:nrow(sweep_results)

no.samples <- 5000
cap_cases <- 2000
max_days <- 300

# # # Save final combined results
saveRDS(sweep_results, file = "data-raw/res_Aug_complete.rds")


#########################
# Missed Chains (Fig 3a and b):

# 5 initial cases

rm(list = ls())
devtools::load_all()

set.seed(200518)
no.samples <- 5000
seed.cases <- 5

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.06), # probability of self-isolation if symptomatic, assume == hospitalisation proportion
    delay_scale = 5.95402 # time from onset to self-isolation (i.e. hospitalisation)
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = 1,
  #self_report = proportion of self-isolating cases that self-report into tracing system
  # (assume only hospitalised cases "isolate"/detected, i.e. all isolating cases are reported)
  self_report = c(1),
  test_delay = c(0), #time from isolation to test result
  sensitivity = c(0), #sensitivity of test
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = seed.cases) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 50
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

#+ full_run
tic()
## Run parameter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE,
                                         earlyOut = TRUE)
toc()

sweep_results$sims[[1]] <- sweep_results$sims[[1]] %>% group_by(sim) %>%
  mutate(early_missed = c(unique(early_missed),rep(NA,7))) %>%
  mutate(first_iso = c(unique(first_iso),rep(NA,7))) %>%
  mutate(index_R0 = paste(sweep_results$index_R0[1])) %>%
  ungroup()
sweep_results$sims[[2]] <- sweep_results$sims[[2]] %>% group_by(sim) %>%
  mutate(early_missed = c(unique(early_missed),rep(NA,7))) %>%
  mutate(first_iso = c(unique(first_iso),rep(NA,7))) %>%
  mutate(index_R0 = paste(sweep_results$index_R0[2])) %>%
  ungroup()
sweep_results$sims[[3]] <- sweep_results$sims[[3]] %>% group_by(sim) %>%
  mutate(early_missed = c(unique(early_missed),rep(NA,7))) %>%
  mutate(first_iso = c(unique(first_iso),rep(NA,7))) %>%
  mutate(index_R0 = paste(sweep_results$index_R0[3])) %>%
  ungroup()

saveRDS(sweep_results, file = "data-raw/res_Aug_missedChains_5cases.rds")

# 100 initial cases

rm(list = ls())
devtools::load_all()

set.seed(200518)
no.samples <- 5000
seed.cases <- 100

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.06), # probability of self-isolation if symptomatic, assume == hospitalisation proportion
    delay_scale = 5.95402 # time from onset to self-isolation (i.e. hospitalisation)
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = 1,
  #self_report = proportion of self-isolating cases that self-report into tracing system
  # (assume only hospitalised cases "isolate"/detected, i.e. all isolating cases are reported)
  self_report = c(1),
  test_delay = c(0), #time from isolation to test result
  sensitivity = c(0), #sensitivity of test
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = seed.cases) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 50
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

#+ full_run
tic()
## Run parameter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE,
                                         earlyOut = TRUE)
toc()

sweep_results$sims[[1]] <- sweep_results$sims[[1]] %>% group_by(sim) %>%
  mutate(early_missed = c(unique(early_missed),rep(NA,7))) %>%
  mutate(first_iso = c(unique(first_iso),rep(NA,7))) %>%
  mutate(index_R0 = paste(sweep_results$index_R0[1])) %>%
  ungroup()
sweep_results$sims[[2]] <- sweep_results$sims[[2]] %>% group_by(sim) %>%
  mutate(early_missed = c(unique(early_missed),rep(NA,7))) %>%
  mutate(first_iso = c(unique(first_iso),rep(NA,7))) %>%
  mutate(index_R0 = paste(sweep_results$index_R0[2])) %>%
  ungroup()
sweep_results$sims[[3]] <- sweep_results$sims[[3]] %>% group_by(sim) %>%
  mutate(early_missed = c(unique(early_missed),rep(NA,7))) %>%
  mutate(first_iso = c(unique(first_iso),rep(NA,7))) %>%
  mutate(index_R0 = paste(sweep_results$index_R0[3])) %>%
  ungroup()

saveRDS(sweep_results, file = "data-raw/res_Aug_missedChains_100cases.rds")

#########################
# Perfect Tracing (Fig 2)

rm(list = ls())
devtools::load_all()

set.seed(200518)
no.samples <- 5000

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.9),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = 1,
  self_report = c(0.5,1),
  test_delay = c(2), #time from isolation to test result
  sensitivity = c(0.65,0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)

#+ full_run
tic()
## Run parameter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE,
                                         earlyOut = FALSE)
toc()

saveRDS(sweep_results, file = "data-raw/res_Aug_perfectTracing.rds")

