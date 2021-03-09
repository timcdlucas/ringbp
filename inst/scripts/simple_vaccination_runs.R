# Running the model and storing outputs

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
library(svglite)

#########################

rm(list = ls())
devtools::load_all()
options(future.rng.onMisuse="ignore")

set.seed(210115)

no.samples <- 5000

index_R0 <- 1.5 #pre-vaccine
prop.asym <- 0.31 #baseline (pre-vaccine)
prop.vacc <- 0.3 #vaccine coverage (1st dose, as of 27th Feb 2021)

prop.asym_vacc <- prop.vacc + prop.asym*(1-prop.vacc) #including vaccine

index_R0_vacc <- index_R0*(1-prop.vacc)

#Scenario 1: no impact on transmission i.e. increase in asymptomatic proportion

scenarios1 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.7),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 0.5,
  max_quar_delay = c(0.5),
  index_R0 = c(1.5),
  prop.asym = prop.asym_vacc,
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.65),
  test_delay = c(0.5), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(TRUE,FALSE)) %>%
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
                                  disp.com = 0.23,
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


# #+ writeout
saveRDS(sweep_results1, file = "data-raw/res_Feb_vacc_1.rds")


#Scenario 2: completely stop transmission i.e. reduce R

scenarios2 <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.7),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 17.773185,
  inf_rate = 1.388388,
  inf_shift = 12.978985,
  min_quar_delay = 0.5,
  max_quar_delay = c(0.5),
  index_R0 = index_R0_vacc,
  prop.asym = 0.31,
  control_effectiveness = seq(0, 0.8, 0.2),
  self_report = c(0.5),
  iso_adhere = c(0.65),
  test_delay = c(0.5), #time from isolation to test result
  sensitivity = c(0.95), #percent of cases detected
  precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
  num.initial.cases = c(5),
  test_asym = c(FALSE)) %>%
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
                                  disp.com = 0.23,
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


# #+ writeout
saveRDS(sweep_results2, file = "data-raw/res_Feb_vacc_2.rds")


sweep_results <- as_tibble(rbind(sweep_results1,sweep_results2))
sweep_results$scenario <- 1:nrow(sweep_results)


sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  dplyr::ungroup(scenario)

sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(mean_Re = calc_R(sims[[1]])) %>%
  dplyr::ungroup(scenario)

sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(sd_Re = calc_R_sd(sims[[1]])) %>%
  dplyr::ungroup(scenario)

res <- sweep_results

lower <- rep(NA,nrow(res))
upper <- rep(NA,nrow(res))

for(i in 1:nrow(res)){
  out <- prop.test(res$pext[i]*no.samples,no.samples,correct=TRUE)
  CIs <- as.numeric(unlist(out[6]))
  lower[i] <- CIs[1]
  upper[i] <- CIs[2]
}

res <- res %>% mutate(lower = lower,
                      upper = upper)
lower <- c()
upper <- c()

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Fig1 <- res %>%
  filter(test_asym==FALSE) %>%
  mutate(index_R0 = factor(index_R0, labels = c('Prevents transmission','Asymptomatic carrier'))) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour=index_R0, linetype=index_R0)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(7,2)],name="Vaccine scenario:") +
  ggplot2::scale_linetype_manual(values = c(2,1),name="Vaccine scenario:") +
  geom_line(lwd=1.1) +
  geom_point() +
  geom_linerange(aes(control_effectiveness,ymax=1-lower,ymin=1-upper),show.legend=FALSE) +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.1,0.32),legend.title=element_text(size=14)) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.5))


Fig2 <- res %>%
  filter(test_asym==FALSE) %>%
  mutate(index_R0 = factor(index_R0, labels = c('Prevents transmission','Creates asymptomatic carriers'))) %>%
  ggplot(aes(control_effectiveness, 100*(1.5-mean_Re)/1.5, colour=index_R0, linetype=index_R0)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(7,2)],name="Vaccine scenario:") +
  ggplot2::scale_linetype_manual(values = c(2,1),name="Vaccine scenario:") +
  geom_line(lwd=1.1) +
  geom_point() +
  #geom_linerange(aes(control_effectiveness,ymax=mean_Re+sd_Re,ymin=mean_Re-sd_Re),show.legend=FALSE) +
  #facet_rep_grid(index_R0 ~ test_asym,scales='free', repeat.tick.labels = 'all') +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.1,0.15),legend.title=element_text(size=14)) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="% reduction in R") +
  ylim(0,80)


Fig3 <- res %>%
  filter(index_R0==1.5) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour=test_asym, linetype=test_asym)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(7,2)],name="Asymptomatic testing:") +
  ggplot2::scale_linetype_manual(values = c(2,1),name="Asymptomatic testing:") +
  geom_line(lwd=1.1) +
  geom_point() +
  geom_linerange(aes(control_effectiveness,ymax=1-lower,ymin=1-upper),show.legend=FALSE) +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.1,0.32),legend.title=element_text(size=14)) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.5))

Fig4 <- res %>%
  filter(index_R0==1.5) %>%
  ggplot(aes(control_effectiveness, 100*(1.5-mean_Re)/1.5, colour=test_asym, linetype=test_asym)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(7,2)],name="Asymptomatic testing:") +
  ggplot2::scale_linetype_manual(values = c(2,1),name="Asymptomatic testing:") +
  geom_line(lwd=1.1) +
  geom_point() +
  #geom_linerange(aes(control_effectiveness,ymax=mean_Re+sd_Re,ymin=mean_Re-sd_Re),show.legend=FALSE) +
  #facet_rep_grid(index_R0 ~ test_asym,scales='free', repeat.tick.labels = 'all') +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.1,0.15),legend.title=element_text(size=14)) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="% reduction in R") +
  ylim(0,80)







#
#
# delay = c("Adherence")
# delay_shape = c(0.7)
# delay_scale =
# inc_meanlog = 1.434065
# inc_sdlog = 0.6612
# inf_shape = 17.773185
# inf_rate = 1.388388
# inf_shift = 12.978985
# min_quar_delay = 1
# max_quar_delay = 1
# r0community = 1.5
# prop.asym = c(0.31)
# control_effectiveness = 0.6
# self_report = c(0.5)
# iso_adhere = c(0.65)
# test_delay = c(1)
# sensitivity = c(0.65) #percent of cases detected
# precaution = c(0) #this could be between 0 and 7? Number of days stay in isolation if negative test
# num.initial.cases = c(50)
# r0isolated = 0
# disp.iso = 1
# disp.com = 0.23
# quarantine = TRUE
#
#
#
# set.seed(210115)
# no.samples <- 10000

