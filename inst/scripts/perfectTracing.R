knitr::opts_chunk$set(cache = TRUE, fig.width = 8, fig.height = 5, cache.lazy = FALSE)

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

devtools::load_all()

res <- readRDS("data-raw/res_20200524_perfectTracing.rds")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# boxplots for 100% contact tracing
res <- res %>% group_by(scenario) %>%
  mutate(trace_stats = list(trace_outs(sims[[1]]))) %>%
  ungroup()

res <- res %>% dplyr::filter(control_effectiveness == 1) %>%
  dplyr::filter(max_quar_delay == 1) %>%
  dplyr::filter(precaution == 7) %>%
  dplyr::filter(test_delay == 2) %>%
  dplyr::filter(sensitivity != 0) %>%
  dplyr::filter(self_report != 0.1) %>%
  dplyr::mutate(index_R0 = factor(index_R0, labels=c("1.1","1.3","1.5"))) %>%
  dplyr::mutate(sensitivity = factor(sensitivity, labels=c("65% sensitive","95%")))  %>%
  dplyr::mutate(self_report = factor(self_report, labels=c("50% self reporting","100%")))

res <- res %>% unnest(trace_stats) %>%


Fig5B <- res %>% filter(cases>=20) %>%
  mutate(precaution = factor(precaution,labels=" ")) %>%
  ggplot(aes(index_R0,positive/cases,fill=index_R0,colour=index_R0)) + geom_boxplot(alpha=0.2) +
  scale_fill_manual(values = cbPalette[c(4,2,7)],name="",guide=FALSE) +
  scale_colour_manual(values = cbPalette[c(4,2,7)],name="",guide=FALSE) +
  facet_grid(sensitivity ~ self_report) +
  ggplot2::labs(tag = "B",
                x = TeX("Index $\\R_s$"),
                y = 'proportion cases detected') +
  theme_minimal(base_size=18)


################################################
### Could be used to re-run these results:

set.seed(200518)
no.samples <- 3000

scenarios <- tidyr::expand_grid(
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
  control_effectiveness = 1,
  self_report = c(0.1,0.5,1),
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

saveRDS(sweep_results, file = "data-raw/res_20200524_perfectTracing.rds")
