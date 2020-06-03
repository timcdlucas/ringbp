#'---
#'output:
#'  pdf_document:
#'    number_sections: true
#'title: "Further analysis of COVID branching process"
#'author: Tim Lucas and Emma Davis
#'fontsize: 8pt
#'geometry: margin=0.5in
#'---

#' # Major model update 1
#'
#' - Quarantine now applies to asymptomatics
#' - Examine delay for contact tracing
#' - Change onset delay to either a 1 day delay or non-adherence.
#' - People that are quarantined have adherence of 1.
#' -

#+setup, echo = TRUE, cache = FALSE

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
library(svglite)

devtools::load_all()

# git2r::revparse_single('.',"HEAD")$sha

set.seed(200518)

#' Delay shape is adherence probability
#'
#' Cap cases was chosen in a seperate analysis (choose_cap.R or something.)
no.samples <- 3000

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
  inf_shape = 2.115779,
  inf_rate = 0.6898583,
  inf_shift = 3,
  min_quar_delay = 1,
  max_quar_delay = c(1),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = seq(0.4, 1, 0.2),
  self_report = c(0.5),
  test_delay = c(0,2), #time from isolation to test result
  sensitivity = c(0.65,0.95), #percent of cases detected
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

#+ full_run
tic()
## Run parameter sweep
sweep_results1 <- ringbp::parameter_sweep(scenarios1,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE)
toc()


# #+ writeout
# saveRDS(sweep_results1, file = "data-raw/res_20200518_1.rds")
rm(list=ls())
Sys.sleep(180)
# tic()
# sweep_results1 <- readRDS("data-raw/res_20200518_1.rds")
# toc()
##################################################################

set.seed(200518)
no.samples <- 3000

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
  control_effectiveness = seq(0.4, 1, 0.2),
  self_report = c(0.1),
  test_delay = c(2), #time from isolation to test result
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

tic()
## Run parameter sweep
sweep_results2 <- ringbp::parameter_sweep(scenarios2,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE)

toc()

# #+ writeout
# saveRDS(sweep_results2, file = "data-raw/res_20200518_2.rds")
rm(list=ls())
Sys.sleep(180)
##################################################################

set.seed(200518)
no.samples <- 3000

scenarios3 <- tidyr::expand_grid(
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
  max_quar_delay = c(4),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = seq(0.4, 1, 0.2),
  self_report = c(0.5),
  test_delay = c(2), #time from isolation to test result
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
sweep_results3 <- ringbp::parameter_sweep(scenarios3,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE)

toc()


# #+ writeout
# saveRDS(sweep_results3, file = "data-raw/res_20200518_3.rds")
rm(list=ls())
Sys.sleep(180)
##################################################################

set.seed(200518)
no.samples <- 3000

scenarios4 <- tidyr::expand_grid(
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
  max_quar_delay = c(4),
  index_R0 = c(1.1,1.3,1.5),
  prop.asym = c(0.4),
  control_effectiveness = seq(0.4, 1, 0.2),
  self_report = c(0.5),
  test_delay = c(2), #time from isolation to test result
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
sweep_results4 <- ringbp::parameter_sweep(scenarios4,
                                          sim_fn = sim_with_params,
                                          samples = no.samples,
                                          show_progress = TRUE)

toc()


# #+ writeout
# saveRDS(sweep_results4, file = "data-raw/res_20200518_4.rds")
rm(list=ls())
Sys.sleep(180)
##################################################################

set.seed(200518)
no.samples <- 3000

scenarios5 <- tidyr::expand_grid(
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
  control_effectiveness = seq(0.4, 1, 0.2),
  self_report = c(0.5),
  test_delay = c(0), #time from isolation to test result
  sensitivity = c(0), #percent of cases detected
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
sweep_results5 <- ringbp::parameter_sweep(scenarios5,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE)

toc()

# #+ writeout
# saveRDS(sweep_results5, file = "data-raw/res_20200518_5.rds")

##################################################################

# # Load in separate 5 sets of scenarios and combine
# sweep_results1 <- readRDS("data-raw/res_20200518_1.rds")
# sweep_results2 <- readRDS("data-raw/res_20200518_2.rds")
# sweep_results3 <- readRDS("data-raw/res_20200518_3.rds")
# sweep_results4 <- readRDS("data-raw/res_20200518_4.rds")
# sweep_results5 <- readRDS("data-raw/res_20200518_5.rds")
# sweep_results <- rbind(sweep_results1,sweep_results2,sweep_results3,sweep_results4, sweep_results5)
#
# # Clear redundant data frames from working memory
# sweep_results1 = sweep_results2 = sweep_results3 = sweep_results4 = sweep_results5 = c()
#
# # Expand results for no testing so they can be filtered for plotting later on
# # Same results hold for precaution =0,7 and test delay =0,2
# temp1 <- sweep_results %>% filter(sensitivity==0) %>%
#   mutate(precaution := 7)
# temp2 <- sweep_results %>% filter(sensitivity==0) %>%
#   mutate(precaution := 7) %>%
#   mutate(test_delay := 2)
# temp3 <- sweep_results %>% filter(sensitivity==0) %>%
#   mutate(test_delay := 2)
# temp <- rbind(temp1,temp2,temp3)
# temp$scenario <- 144 + (1:nrow(temp))
# sweep_results <- rbind(sweep_results,temp)
# sweep_results$scenario[1:180] <- 1:180
#
# # Save final combined results
# saveRDS(sweep_results, file = "data-raw/res_20200518_complete.rds")

# Plot figure 2:  --------------------------------------------------------
# Parameter distributions (incubation, generation interval etc.)

ringbp::make_figure_2()

# Load in results  -------------------------------------------------------

# Load in pre-saved results
sweep_results <- readRDS("data-raw/res_20200518_complete.rds")
# A colour-blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  #dplyr::mutate(timetotest = list(unlist(sims[[1]]$timetotest))) %>%
  dplyr::ungroup(scenario)


# Fig 4 - Time tested post-exposure

<<<<<<< HEAD
#+ plotsS, eval = TRUE, cache = FALSE, fig.height = 5, fig.width = 9

# A colour-blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

res1 <- res %>%
=======
res1 <- sweep_results %>%
>>>>>>> df7dc743aa84bba2be6f1807c30822e0b7fd247f
  filter(self_report == 0.5) %>%
  filter(precaution == 7) %>%
  filter(sensitivity == 0.65) %>%
  filter(index_R0 == 1.3) %>%
  filter(control_effectiveness == '0.6')

res1 <- res1 %>%
  dplyr::group_by(scenario) %>%
  #dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  dplyr::mutate(timetotest = list(unlist(sims[[1]]$timetotest))) %>%
  dplyr::ungroup(scenario)

falseNeg <- read.csv('data-raw/FalseNegative_kucirka.csv')
#saveRDS(res1,"data-raw/Fig4.rds")

h1 <- data.frame(y=c(unlist(res1$timetotest[1]),unlist(res1$timetotest[2])),delay=c(rep("2 days",length(unlist(res1$timetotest[1]))),rep("0 days",length(unlist(res1$timetotest[2]))))) %>%
  ggplot() +
  geom_density(alpha=0.2,aes(y,y=..scaled..,fill=delay,colour=delay)) + theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ggplot2::scale_colour_manual(values = cbPalette[c(2,3)],name="Test delay\n (density)") +
  ggplot2::scale_fill_manual(values = cbPalette[c(2,3)],name="Test delay\n (density)") +
  xlim(c(0,15)) +
  geom_point(data=falseNeg, aes(x=Day,y=1-Mean)) +
  geom_linerange(data=falseNeg,aes(x=Day,ymax=1-Lower,ymin=1-Upper)) +
  theme_minimal(base_size = 18) +
  xlab('Time tested (days post-exposure)') +
  ylab('Sensitivity (Kucirka et al.)')


# Supp Fig7 A and B

res <- sweep_results %>%
  filter(precaution == 7) %>%
  filter(test_delay == 2) %>%
  filter(sensitivity == 0.65)

saveRDS(res,'data-raw/FigS7.rds')

Fig7A <- sweep_results %>%
  filter(max_quar_delay == 1) %>%
  filter(precaution == 7) %>%
  filter(test_delay == 2) %>%
  filter(sensitivity == 0.65) %>%
  mutate(sensitivity = factor(sensitivity, labels = c('sensitivity = 65%'))) %>%
  mutate(self_report = factor(self_report, labels = c('10% self-reporting','50%'))) %>%
  mutate(index_R0 = factor(index_R0)) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = index_R0)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
  geom_line() +
  geom_point() +
  facet_grid(self_report ~ sensitivity) +
  theme_minimal(base_size = 18) +
  ggplot2::theme(legend.position = "bottom") +
  labs(tag="A",x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.3))

Fig7B <- sweep_results %>%
  filter(self_report == 0.5) %>%
  filter(precaution == 7) %>%
  filter(test_delay == 2) %>%
  filter(sensitivity == 0.65) %>%
  mutate(sensitivity = factor(sensitivity, labels = c('sensitivity = 65%'))) %>%
  mutate(max_quar_delay = factor(max_quar_delay, labels = c('1 day trace delay', '4 days'))) %>%
  mutate(index_R0 = factor(index_R0)) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = index_R0)) +
  geom_line() +
  geom_point() +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
  facet_grid(max_quar_delay ~ sensitivity) +
  theme_minimal(base_size = 18) +
  ggplot2::theme(legend.position = "bottom") +
  labs(tag="B",x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.3))

Fig7A + Fig7B

# Fig 3 A and B
res <- sweep_results %>%
  filter(self_report == 0.5) %>%
  filter(max_quar_delay == 1)

#saveRDS(res,'data-raw/Fig3.rds')

Fig3B <- res %>%
  filter(self_report == 0.5) %>%
  filter(max_quar_delay == 1) %>%
  filter(sensitivity == 0.65) %>%
  mutate(test_delay = factor(test_delay, labels = c('instant test','2 day delay'))) %>%
  mutate(precaution = factor(precaution, labels = c('immediate release if negative', 'minimum 7 days'))) %>%
  mutate(index_R0 = factor(index_R0)) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = index_R0)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$:")) +
  geom_line() +
  geom_point() +
  facet_grid(test_delay ~ precaution) +
  theme_minimal(base_size = 18) +
  ggplot2::theme(legend.position = "bottom") +
  labs(tag="B",x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.3))

Fig3A <- res %>%
  filter(self_report == 0.5) %>%
  filter(max_quar_delay == 1) %>%
  filter(index_R0 == 1.3) %>%
  mutate(test_delay = factor(test_delay, labels = c('instant test','2 day delay'))) %>%
  mutate(precaution = factor(precaution, labels = c('immediate release if negative', 'minimum 7 days'))) %>%
  mutate(sensitivity = factor(sensitivity, labels = c('No testing','65%','95%'))) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = sensitivity)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(1,3,6)],name="Test sensitivity:") +
  geom_line() +
  geom_point() +
  facet_grid(test_delay ~ precaution) +
  theme_minimal(base_size = 18) +
  ggplot2::theme(legend.position = "bottom") +
  labs(tag="A",x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.17))

Fig3A + Fig3B

# Manipulate data for further plots

res2 <- list()
week_range <- 40:42

sweep_results <- sweep_results %>%
  filter(self_report == 0.5,
         test_delay == 2)

for(i in seq_len(nrow(sweep_results))){
  #print(i)
  tmp <- sweep_results$sims[i][[1]]
  tmp <-
    tmp %>%
    dplyr::group_by(sim) %>% # group by simulation run
    mutate(max_weekly = max(weekly_cases),
           time_to_size = which(cumulative>=500)[1], #time to reach 500 cases (weeks)
           total = max(cumulative)) %>%
    dplyr::filter(week %in% week_range) %>%
    dplyr::summarise(extinct =
                       ifelse(all(weekly_cases == 0 &
                                    cumulative < cap_cases),
                              1, 0),
                     max_weekly = max(max_weekly),
                     time_to_size = min(time_to_size),
                     total = max(total)) %>%
    dplyr::ungroup()
  tmp <-
    tmp %>%
    mutate(index_R0 = sweep_results$index_R0[i],
           control_effectiveness = sweep_results$control_effectiveness[i],
           max_quar_delay = sweep_results$max_quar_delay[i],
           precaution = sweep_results$precaution[i],
           sensitivity = sweep_results$sensitivity[i])

  res2[[i]] <- tmp
}
res2 <- do.call(rbind, res2)

# we want:
# total outbreaks / n
total_cumulative_distr <-
  res2 %>%
  mutate(total = ifelse(total > 2000, 2000, total)) %>%
  group_by(index_R0, control_effectiveness, max_quar_delay, precaution, sensitivity) %>%
  do(res = tibble(cumdistr = nrow(.) * ecdf(.$total)(4:2000),
                  total = 4:2000,
                  outbreaks = nrow(.) - sum(.$extinct),
                  runs = nrow(.),
                  max_quar_delay = .$max_quar_delay[1],
                  index_R0 = .$index_R0[1],
                  precaution = .$precaution[1],
                  sensitivity = .$sensitivity[1],
                  control_effectiveness = .$control_effectiveness[1],
                  poutbreak = (outbreaks) / (runs - cumdistr)))


total_cumulative_distr <- do.call(rbind, total_cumulative_distr$res) %>%
  filter(index_R0 != 1.1) %>%
  mutate(index_R0 = factor(index_R0, labels = c('R0 = 1.3','1.5'))) %>%
  mutate(precaution = factor(precaution, labels = c('immediate release', '7 days'))) %>%
  mutate(sensitivity = factor(sensitivity, labels = c('No testing','65% sensitive','95%'))) %>%
  mutate(max_quar_delay = factor(max_quar_delay, labels = c('1 day trace delay', '4 days'))) %>%
  filter(outbreaks != 0)

# Fig 5A

T1 <- total_cumulative_distr %>% filter(sensitivity=="65% sensitive") %>%
  filter(precaution=="7 days")

saveRDS(T1,'data-raw/Fig5A.rds')

Fig5A <- ggplot(T1,
         aes(total, poutbreak, colour = factor(control_effectiveness), group = factor(control_effectiveness))) +
    geom_line() +
    facet_grid(max_quar_delay ~ index_R0) +
    scale_colour_manual(values = cbPalette) +
    ylab('Prob. large outbreak') +
    guides(colour=guide_legend(title="Prop. Traced")) +
    theme_minimal(base_size = 18) +
    ggplot2::theme(legend.position = "top") +
    labs(tag="A",x='total cases so far',y="Prob. large outbreak") +
    xlim(c(0,1000)) +
    ylim(c(0,1))


# Fig 6: Plot showing risk of different outbreak sizes occurring

res3 <- res2 %>% filter(sensitivity==0.65) %>%
  #filter(max_quar_delay==1) %>%
  filter(precaution==7) %>%
  mutate(total = pmin(total,2000))

res3 <- res3 %>%
  group_by(control_effectiveness, index_R0, max_quar_delay) %>%
  mutate(x := total[order(total)]) %>%
  mutate(y := 1-seq_along(total)/length(total)) %>%
  ungroup

res3 <- res3 %>% group_by(control_effectiveness, index_R0,x,max_quar_delay) %>%
  mutate(y=max(y)) %>%
  ungroup()

saveRDS(res3,'Fig6.rds')

Fig6 <- res3 %>% mutate(index_R0 = factor(index_R0, labels=c("1.1","1.3","1.5"))) %>%
    mutate(control_effectiveness = factor(control_effectiveness, labels=c("Prop. traced 40%","60%","80%","100%"))) %>%
    mutate(max_quar_delay = factor(max_quar_delay, labels=c("1 day trace delay","4 days"))) %>%
  ggplot(aes(x,y,colour=index_R0)) + geom_line(size=1.2) +
  scale_colour_manual(values = cbPalette[c(4,2,7)]) +
  facet_grid(max_quar_delay ~ control_effectiveness) +
  xlim(c(5,800)) + ylim(c(0,0.5)) +
  #scale_x_continuous(breaks=c(5,500,1000)) +
  theme_minimal(base_size = 18) +
  ggplot2::theme(legend.position = "bottom") +
  xlab('outbreak size, X') +
  ylab('risk of outbreak larger than X') +
  geom_abline(intercept=0.05,slope=0,colour=cbPalette[1],linetype=2)

Fig6

ggsave('data-raw/Fig6.pdf')
ggsave('data-raw/Fig6.svg')

#################################################################
# Getting numbers for paper
#################################################################

Rs <- sweep_results %>% filter(max_quar_delay==1 & sensitivity==0.65 & self_report==0.5 &
                           control_effectiveness==0.8) %>% .$index_R0
tactic <- sweep_results %>% filter(max_quar_delay==1 & sensitivity==0.65 & self_report==0.5 &
                                 control_effectiveness==0.8) %>% .$precaution
testdelays <- sweep_results %>% filter(max_quar_delay==1 & sensitivity==0.65 & self_report==0.5 &
                                 control_effectiveness==0.8) %>% .$test_delay
probOutb <- 1-(sweep_results %>% filter(max_quar_delay==1 & sensitivity==0.65 & self_report==0.5 &
                                         control_effectiveness==0.8) %>% .$pext)
data.frame(Rs,tactic,testdelays,probOutb)

Rs <- sweep_results %>% filter(max_quar_delay==1 & sensitivity==0.65 & self_report==0.5 &
                                 control_effectiveness==0.8 & precaution==7) %>% .$index_R0
testdelays <- sweep_results %>% filter(max_quar_delay==1 & sensitivity==0.65 & self_report==0.5 &
                                         control_effectiveness==0.8 & precaution==7) %>% .$test_delay
probOutb <- 1-(sweep_results %>% filter(max_quar_delay==1 & sensitivity==0.65 & self_report==0.5 &
                                          control_effectiveness==0.8 & precaution==7) %>% .$pext)
data.frame(Rs,testdelays,probOutb)


temp <- res3 %>% filter(index_R0==1.5,control_effectiveness==1,max_quar_delay==1,precaution==7,sensitivity==0.65)
temp$index_R0 <- c()
temp$control_effectiveness <- c()
temp$max_quar_delay <- c()
temp$precaution <- c()
temp$sensitivity <- c()

#################################################################
# END OF RELEVANT PLOTTING
#################################################################



# plots looking at number traced versus proportion traced

res5 <- res_trace %>%  group_by(scenario) %>%
  mutate(avg_test = mean(trace_stats[[1]]$tested),
         avg_pos = mean(trace_stats[[1]]$positive),
         avg_iso = mean(trace_stats[[1]]$isolated),
         avg_rel = mean(trace_stats[[1]]$released),
         avg_case = mean(trace_stats[[1]]$cases)) %>%
    ungroup()

g_test <- res5 %>%
  dplyr::filter(self_report == 0.5) %>%
  dplyr::filter(precaution == 7) %>%
  dplyr::filter(max_quar_delay == 1) %>%
  dplyr::filter(test_delay == 2) %>%
  dplyr::filter(sensitivity == 0.65) %>%
  dplyr::mutate(index_R0 = factor(index_R0)) %>%
  ggplot(aes(control_effectiveness, avg_test,colour=index_R0)) +
    ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$"),guide=FALSE) +
    geom_line() + geom_point() +
    xlab('') +
    theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold"),legend.position = "bottom") +
    ylab('Average total cases tested') +
    ylim(c(0,525))

g_iso <- res5 %>%
  dplyr::filter(self_report == 0.5) %>%
  dplyr::filter(precaution == 7) %>%
  dplyr::filter(max_quar_delay == 1) %>%
  dplyr::filter(test_delay == 2) %>%
  dplyr::filter(sensitivity == 0.65) %>%
  dplyr::mutate(index_R0 = factor(index_R0)) %>%
  ggplot(aes(control_effectiveness, avg_iso,colour=index_R0)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$"),guide=FALSE) +
  geom_line() + geom_point() +
  xlab('Contact tracing coverage') +
  theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ylab('Average total cases isolated') +
  ylim(c(0,525))

g_rel <- res5 %>%
  dplyr::filter(self_report == 0.5) %>%
  dplyr::filter(precaution == 7) %>%
  dplyr::filter(max_quar_delay == 1) %>%
  dplyr::filter(test_delay == 2) %>%
  dplyr::filter(sensitivity == 0.65) %>%
  dplyr::mutate(index_R0 = factor(index_R0)) %>%
  ggplot(aes(control_effectiveness, avg_rel,colour=index_R0)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$"),guide=FALSE) +
  geom_line() + geom_point() +
  xlab('Contact tracing coverage') +
  theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ylab('Average number of cases released early')

g_case <- res5 %>%
  dplyr::filter(self_report == 0.5) %>%
  dplyr::filter(precaution == 7) %>%
  dplyr::filter(max_quar_delay == 1) %>%
  dplyr::filter(test_delay == 2) %>%
  dplyr::filter(sensitivity == 0.65) %>%
  dplyr::mutate(index_R0 = factor(index_R0)) %>%
  ggplot(aes(control_effectiveness, avg_case,colour=index_R0)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
  geom_line() + geom_point() +
  xlab('') +
  theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ylab('Average total cases')

(g_test + g_case | g_rel + g_iso)

# Boxplots?
# res3 %>% filter(sensitivity==0.65) %>%
#   mutate(control_effectiveness = factor(control_effectiveness,labels=c("Prop. traced 40%","60%","80%","100%"))) %>%
#   mutate(index_R0 = factor(index_R0, labels=c("1.1","1.3", "1.5"))) %>%
#   mutate(max_quar_delay = factor(max_quar_delay, labels=c("1 day trace delay","4 days"))) %>%
#   ggplot(aes(control_effectiveness,max_weekly)) + geom_boxplot() +
#   facet_grid(index_R0 ~ max_quar_delay) +
#   scale_y_log10()

#+ plots3

testRes <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(timetotest = list(unlist(sims[[1]]$timetotest))) %>%
  dplyr::ungroup()

# Histogram of how long it takes to reach 500 cases (weeks)
# Doesn't matter which variables you look at, looks like roughly the same distribution of times
res2 %>%
  filter(control_effectiveness == "0.6") %>%
  filter(sensitivity==0.65) %>%
  filter(index_R0 == 1.5) %>%
  ggplot(aes(time_to_size)) + geom_histogram(aes(y=..density..),breaks=1:30,
                                             na.rm=T, colour=cbPalette[2],fill=cbPalette[2]) +
  facet_grid(precaution ~ max_quar_delay) +
  scale_colour_manual(values = cbPalette) +
  ggtitle('Time to reach 500 cases') +
  xlab('Time (weeks)')

# boxplots for 100% contact tracing
res_trace <- res %>% group_by(scenario) %>%
  mutate(trace_stats = list(trace_outs(sims[[1]]))) %>%
  ungroup()

res4 <- res_trace %>% dplyr::filter(control_effectiveness == 1) %>%
  dplyr::filter(self_report == 0.5) %>%
  dplyr::filter(max_quar_delay == 1) %>%
  dplyr::filter(precaution == 7) %>%
  dplyr::filter(test_delay == 2) %>%
  dplyr::filter(sensitivity != 0) %>%
  dplyr::mutate(index_R0 = factor(index_R0, labels=c("1.1","1.3","1.5"))) %>%
  dplyr::mutate(sensitivity = factor(sensitivity, labels=c("65% sensitive","95%")))

res4 <- res4 %>% unnest(trace_stats)

res4 %>% filter(cases>=20) %>%
  mutate(precaution = factor(precaution,labels=" ")) %>%
  ggplot(aes(index_R0,positive/cases,fill=index_R0)) + geom_boxplot() +
  scale_fill_manual(values = cbPalette[c(4,2,7)],name="",guide=FALSE) +
  facet_grid(precaution ~ sensitivity) +
  ylab('proportion cases detected') +
  xlab(TeX("Index $\\R_s$")) +
  ggplot2::theme(legend.position = "bottom") +
  theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ggtitle('100% of contacts traced and tested')
