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

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sweep_results <- readRDS("data-raw/res_20200527_Re.rds")

sweep_results <- sweep_results %>% group_by(scenario) %>%
  mutate(meanRe = mean(sims[[1]]$effective_r0[which(sims[[1]]$week==42 & sims[[1]]$effective_r0!=0)])) %>%
  mutate(sdRe = sd(sims[[1]]$effective_r0[which(sims[[1]]$week==1 & sims[[1]]$effective_r0!=0)])) %>%
  mutate(medRe = median(sims[[1]]$effective_r0[which(sims[[1]]$week==1 & sims[[1]]$effective_r0!=0)])) %>%
  mutate(lowerRe = quantile(sims[[1]]$effective_r0[which(sims[[1]]$week==1 & sims[[1]]$effective_r0!=0)],0.05)) %>%
  mutate(upperRe = quantile(sims[[1]]$effective_r0[which(sims[[1]]$week==1 & sims[[1]]$effective_r0!=0)],0.95)) %>%
  ungroup()


sweep_results <- sweep_results %>%
  mutate(precaution = factor(precaution,labels=c("immediate release","7 day minimum"))) %>%
  mutate(index_R0 = factor(index_R0,labels=c(1.1,1.3,1.5))) %>%
  mutate(sensitivity = factor(sensitivity, labels=c("65% sensitive")))

sweep_results %>% ggplot(aes(x=control_effectiveness,y=meanRe,colour=index_R0,linetype=precaution)) +
  geom_line() +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$:")) +
  ggplot2::scale_linetype_manual(values = c(2,1),name=TeX("-ve test:")) +
  ylim(c(0,1.5))



# git2r::revparse_single('.',"HEAD")$sha

set.seed(200527)

#' Delay shape is adherence probability
#'
#' Cap cases was chosen in a seperate analysis (choose_cap.R or something.)
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
  inf_shape = 2.115779,
  inf_rate = 0.6898583,
  inf_shift = 3,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = seq(0, 1, 0.2),
  self_report = c(0.5),
  test_delay = c(2), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0,7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(20)) %>%
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


# #+ writeout
saveRDS(sweep_results, file = "data-raw/res_20200527_Re.rds")
