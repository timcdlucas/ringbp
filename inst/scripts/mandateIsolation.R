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

sweep_results <- readRDS("data-raw/res_20200529_iso.rds")

sweep_results <- sweep_results %>%
  mutate(index_R0 = factor(index_R0,labels=c(1.1,1.3,1.5))) %>%
  mutate(iso_scenario = factor(iso_scenario,labels=c('voluntary','mandated'))) %>%
  mutate(contact_adhere = factor(contact_adhere)) %>%
  mutate(delay_shape = factor(delay_shape)) %>%
  mutate(sensitivity = factor(sensitivity,labels="65% sensitive")) %>%
  mutate(iso_adhere = factor(iso_adhere))

sweep_results %>% ggplot(aes(x=control_effectiveness,y=1-pext,linetype=iso_scenario,colour=index_R0)) +
  geom_line() +
  geom_point() +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
  ggplot2::scale_linetype_manual(values = c(2,1),name=TeX("Scenario")) +
  ylim(c(0,1)) +
  xlab('Proportion of contacts traced given a case is compliant to tracing') +
  theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ylab('Prob. of a large outbreak')

sweep_results %>% ggplot(aes(x=control_effectiveness,y=1-pext,colour=index_R0)) +
  geom_line() +
  geom_point() +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
  facet_grid(iso_scenario ~ sensitivity) +
  ylim(c(0,1))


# git2r::revparse_single('.',"HEAD")$sha

set.seed(200529)

#' Delay shape is adherence probability
#'
#' Cap cases was chosen in a seperate analysis (choose_cap.R or something.)
no.samples <- 1000

# Scenario 1: 90% self reporting and contact reporting, 60% isolation  adherence
contact_adhere = 0.9

scenarios1 <- tidyr::expand_grid(
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
  control_effectiveness = contact_adhere*seq(0.4, 1, 0.3),
  self_report = c(1),
  iso_adhere = c(0.6),
  test_delay = c(2), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
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
sweep_results1 <- ringbp::parameter_sweep(scenarios1,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE,
                                          earlyOut = FALSE)
toc()


# Scenario 2: 60% self reporting and contact reporting, 90% isolation  adherence
contact_adhere = 0.6

scenarios2 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.6),
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
  control_effectiveness = contact_adhere*seq(0.4, 1, 0.3),
  self_report = c(1),
  iso_adhere = c(0.9),
  test_delay = c(2), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
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
sweep_results2 <- ringbp::parameter_sweep(scenarios2,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE,
                                         earlyOut = FALSE)
toc()

sweep_results <- rbind(sweep_results1,sweep_results2)

sweep_results1 <- sweep_results1 %>% mutate(contact_adhere = 0.9) %>%
  mutate(control_effectiveness = as.numeric(control_effectiveness)/contact_adhere)
sweep_results2 <- sweep_results2 %>% mutate(contact_adhere = 0.6) %>%
  mutate(control_effectiveness = as.numeric(control_effectiveness)/contact_adhere)
sweep_results <- rbind(sweep_results1,sweep_results2)
sweep_results$scenario <- 1:nrow(sweep_results)
sweep_results$iso_scenario <- c(rep(1,nrow(sweep_results1)),rep(2,nrow(sweep_results1)))

sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  dplyr::ungroup(scenario)

# #+ writeout
saveRDS(sweep_results, file = "data-raw/res_20200529_iso.rds")

