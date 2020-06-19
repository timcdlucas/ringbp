


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
no.samples <- 2000

# Scenario 1: 90% self reporting and contact reporting, 60% isolation  adherence
contact_adhere <- 1

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
  control_effectiveness = seq(0.4, 0.8, 0.2),
  self_report = seq(0.4, 0.9, 0.1),
  iso_adhere = seq(0.4, 0.9, 0.1),
  test_delay = c(2), #time from isolation to test result
  sensitivity = c(0.65), #percent of cases detected
  precaution = c(0), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(20)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())



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



saveRDS(sweep_results1, file = "data-raw/res_20200617_iso.rds")

if(!exists('sweept_results1')){
  sweep_results1 <- readRDS(file = "data-raw/res_20200617_iso.rds")
}

sweep_results <- sweep_results1
rm(sweep_results1)
# 
# sweep_results <- rbind(sweep_results1,sweep_results2)
# 
# sweep_results1 <- sweep_results1 %>% mutate(contact_adhere = 0.9) %>%
#   mutate(control_effectiveness = as.numeric(control_effectiveness)/contact_adhere)
# sweep_results2 <- sweep_results2 %>% mutate(contact_adhere = 0.6) %>%
#   mutate(control_effectiveness = as.numeric(control_effectiveness)/contact_adhere)
# sweep_results <- rbind(sweep_results1,sweep_results2)
# sweep_results$scenario <- 1:nrow(sweep_results)
# sweep_results$iso_scenario <- c(rep(1,nrow(sweep_results1)),rep(2,nrow(sweep_results1)))

sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  dplyr::ungroup(scenario)

# #+ writeout
#saveRDS(sweep_results, file = "data-raw/res_20200529_iso.rds")






cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


sweep_results %>% 
  ggplot(aes(self_report, iso_adhere, fill = 1 - pext,)) + 
  geom_tile() +
  facet_grid(control_effectiveness ~ index_R0) +
  scale_fill_viridis_c() +
  labs(fill = 'Risk')


r01 <- 
sweep_results %>% 
  filter(control_effectiveness == "0.6") %>% 
  filter(index_R0 == 1.1) %>% 
  ggplot(aes(self_report, iso_adhere, fill = 1 - pext,)) + 
  geom_tile() +
  scale_fill_viridis_c() +
  ggtitle('R0 = 1.1') +
  labs(fill = 'Risk')


r02 <- 
  sweep_results %>% 
  filter(control_effectiveness == "0.6") %>% 
  filter(index_R0 == 1.3) %>% 
  ggplot(aes(self_report, iso_adhere, fill = 1 - pext,)) + 
  geom_tile() +
  scale_fill_viridis_c() +
  ggtitle('R0 = 1.3') +
  labs(fill = 'Risk')

r03 <- 
  sweep_results %>% 
  filter(control_effectiveness == "0.6") %>% 
  filter(index_R0 == 1.5) %>% 
  ggplot(aes(self_report, iso_adhere, fill = 1 - pext,)) + 
  geom_tile() +
  scale_fill_viridis_c() +
  ggtitle('R0 = 1.5') +
  labs(fill = 'Risk')


(r01 + r02) / (r03 + plot_spacer())





r01 <- 
  sweep_results %>% 
  filter(control_effectiveness == "0.4") %>% 
  filter(index_R0 == 1.1) %>% 
  ggplot(aes(self_report, iso_adhere, fill = 1 - pext,)) + 
  geom_tile() +
  scale_fill_viridis_c() +
  ggtitle('R0 = 1.1') +
  labs(fill = 'Risk')


r02 <- 
  sweep_results %>% 
  filter(control_effectiveness == "0.4") %>% 
  filter(index_R0 == 1.3) %>% 
  ggplot(aes(self_report, iso_adhere, fill = 1 - pext,)) + 
  geom_tile() +
  scale_fill_viridis_c() +
  ggtitle('R0 = 1.3') +
  labs(fill = 'Risk')

r03 <- 
  sweep_results %>% 
  filter(control_effectiveness == "0.4") %>% 
  filter(index_R0 == 1.5) %>% 
  ggplot(aes(self_report, iso_adhere, fill = 1 - pext,)) + 
  geom_tile() +
  scale_fill_viridis_c() +
  ggtitle('R0 = 1.5') +
  labs(fill = 'Risk')


(r01 + r02) / (r03 + plot_spacer())


# 
# sweep_results <- sweep_results %>%
#   mutate(index_R0 = factor(index_R0,labels=c(1.1,1.3,1.5))) %>%
#   mutate(iso_scenario = factor(iso_scenario,labels=c('voluntary','mandated'))) %>%
#   mutate(contact_adhere = factor(contact_adhere)) %>%
#   mutate(delay_shape = factor(delay_shape)) %>%
#   mutate(sensitivity = factor(sensitivity,labels="65% sensitive")) %>%
#   mutate(iso_adhere = factor(iso_adhere))
# 
# sweep_results %>% ggplot(aes(x=control_effectiveness*100,y=1-pext,linetype=iso_scenario,colour=index_R0)) +
#   geom_line() +
#   geom_point() +
#   ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
#   ggplot2::scale_linetype_manual(values = c(2,1),name=TeX("Scenario")) +
#   ylim(c(0,1)) +
#   xlab('Average % contacts traced') +
#   theme_cowplot(font_size=16) +
#   ylab('Prob. of a large outbreak')
# 
# sweep_results %>% ggplot(aes(x=control_effectiveness,y=1-pext,colour=index_R0)) +
#   geom_line() +
#   geom_point() +
#   ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
#   facet_grid(iso_scenario ~ sensitivity) +
#   ylim(c(0,1))



no.samples <- 2000



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
                                          show_progress = TRUE,
                                          earlyOut = FALSE)
toc()



saveRDS(sweep_results2, file = "data-raw/res_20200617_iso2.rds")

