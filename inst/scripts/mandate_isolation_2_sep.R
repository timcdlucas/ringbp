#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# Read in start i and define batch size.
i <- as.numeric(args)

# output path
output_path <- paste0('../data/mandate_iso_q2', i, '.rds')

# functions 
pkg_location <- '../ringbp'


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

devtools::load_all(pkg_location)

# git2r::revparse_single('.',"HEAD")$sha

set.seed(200529)

#' Delay shape is adherence probability
#'
#' Cap cases was chosen in a seperate analysis (choose_cap.R or something.)
no.samples <- 1000

# Scenario 1: 90% self reporting and contact reporting, 60% isolation  adherence
contact_adhere <- 1

# 972 rows
scenarios2 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.9),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 2.115779,
  inf_rate = 0.6898583,
  inf_shift = 3,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = seq(0.4, 0.8, 0.2),
  control_effectiveness_base = seq(0.4, 0.8, 0.2),
  self_report = seq(0.4, 0.9, 0.1),
  iso_adhere = seq(0.4, 0.9, 0.1),
  test_delay = c(2), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(20)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

scenarios2$control_effectiveness <- scenarios2$self_report * scenarios2$control_effectiveness


scenarios2 <- scenarios2[i, ]

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


#+ full_run
tic()
## Run parameter sweep
sweep_results2 <- ringbp::parameter_sweep(scenarios2,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut = FALSE)
toc()



saveRDS(sweep_results2, file = output_path)

