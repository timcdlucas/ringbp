# Code for time to diagnostic test
# IMPORTANT NOTE: Before running uncomment lines 137, 173 and 174 of outbreak_model.R (timetotest definition)

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
  iso_adhere = 1,
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 1,
  max_quar_delay = c(1,4),
  index_R0 = c(1.3),
  prop.asym = c(0.4),
  control_effectiveness = c(0.6),
  self_report = c(0.5),
  test_delay = c(0,2,4), #time from isolation to test result
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
sweep_results <- ringbp::parameter_sweep(scenarios,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE)
toc()


# writeout
saveRDS(sweep_results, file = "data-raw/res_timetotest.rds")



