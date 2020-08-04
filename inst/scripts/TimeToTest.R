# Code for time to test model runs and plots

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

# Plotting:

sweep_results <- readRDS("data-raw/res_timetotest.rds")
falseNeg <- read.csv('data-raw/FalseNegative_kucirka.csv')
cap_cases <- 2000
max_days <- 300

# A colour-blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

res1 <- sweep_results %>%
  filter(index_R0==1.3) %>%
  filter(control_effectiveness==0.6) %>%
  filter(max_quar_delay==1) %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  dplyr::mutate(timetotest = list(unlist(sims[[1]]$timetotest))) %>%
  dplyr::ungroup(scenario)

res2 <- sweep_results %>%
  filter(index_R0==1.3) %>%
  filter(control_effectiveness==0.6) %>%
  filter(max_quar_delay==4) %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  dplyr::mutate(timetotest = list(unlist(sims[[1]]$timetotest))) %>%
  dplyr::ungroup(scenario)

df_h1 <- data.frame(y=c(unlist(res1$timetotest[1]),unlist(res1$timetotest[2]),unlist(res1$timetotest[3])),delay=c(rep("4 days",length(unlist(res1$timetotest[1]))),rep("2 days",length(unlist(res1$timetotest[2]))),rep("0 days",length(unlist(res1$timetotest[3])))))
h1 <- df_h1 %>%
  ggplot() +
  geom_density(alpha=0.2,aes(y,y=..scaled..,fill=delay,colour=delay)) + theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ggplot2::scale_colour_manual(values = cbPalette[c(3,8,1)],name="test delay (density)") +
  ggplot2::scale_fill_manual(values = cbPalette[c(3,8,1)],name="test delay (density)") +
  xlim(c(0,15)) +
  geom_point(data=falseNeg, aes(x=Day,y=1-Mean)) +
  geom_linerange(data=falseNeg,aes(x=Day,ymax=1-Lower,ymin=1-Upper)) +
  geom_line(data=falseNeg, aes(x=seq(0,15,length.out=21),y=rep(0.65,21)),linetype=2,col="grey") +
  theme_cowplot(font_size = 16) +
  ggplot2::theme(legend.position = c(0.8,0.9), legend.title = element_text(size=14)) +
  labs(tag="a",x='Time tested (days post-exposure)',y='Sensitivity (Kucirka et al.)')

h2 <- data.frame(y=c(unlist(res2$timetotest[1]),unlist(res2$timetotest[2]),unlist(res2$timetotest[3])),delay=c(rep("0 days",length(unlist(res2$timetotest[1]))),rep("4 days",length(unlist(res2$timetotest[2]))),rep("2 days",length(unlist(res2$timetotest[3]))))) %>%
  ggplot() +
  geom_density(alpha=0.2,aes(y,y=..scaled..,fill=delay,colour=delay)) + theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ggplot2::scale_colour_manual(values = cbPalette[c(2,3,4)],name="test delay (density)") +
  ggplot2::scale_fill_manual(values = cbPalette[c(2,3,4)],name="test delay (density)") +
  xlim(c(0,15)) +
  geom_point(data=falseNeg, aes(x=Day,y=1-Mean)) +
  geom_linerange(data=falseNeg,aes(x=Day,ymax=1-Lower,ymin=1-Upper)) +
  geom_line(data=falseNeg, aes(x=seq(0,15,length.out=21),y=rep(0.65,21)),linetype=2,col="grey") +
  theme_minimal(base_size = 18) +
  ggplot2::theme(legend.position = "bottom") +
  labs(tag="c",x='time tested (days post-exposure)',y='sensitivity (Kucirka et al.)')

h1/h2

save(file="data-raw/timetotest_plot.Rdata",h1)

h3 <- data.frame(y=c(unlist(res1$timetotest[1]),unlist(res1$timetotest[2]),unlist(res1$timetotest[3])),delay=c(rep("4 days",length(unlist(res1$timetotest[1]))),rep("2 days",length(unlist(res1$timetotest[2]))),rep("0 days",length(unlist(res1$timetotest[3]))))) %>%
  ggplot() +
  stat_ecdf(geom="step",aes(y,colour=delay)) + theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ggplot2::scale_colour_manual(values = cbPalette[c(2,3,4)],name="test delay (cum. density)") +
  xlim(c(0,15)) +
  geom_point(data=falseNeg, aes(x=Day,y=1-Mean)) +
  geom_linerange(data=falseNeg,aes(x=Day,ymax=1-Lower,ymin=1-Upper)) +
  geom_line(data=falseNeg, aes(x=seq(0,15,length.out=21),y=rep(0.65,21)),linetype=2,col="grey") +
  theme_minimal(base_size = 18) +
  ggplot2::theme(legend.position = "bottom") +
  labs(tag="b",x='time tested (days post-exposure)',y='sensitivity (Kucirka et al.)')

h4 <- data.frame(y=c(unlist(res2$timetotest[1]),unlist(res2$timetotest[2]),unlist(res2$timetotest[3])),delay=c(rep("0 days",length(unlist(res2$timetotest[1]))),rep("4 days",length(unlist(res2$timetotest[2]))),rep("2 days",length(unlist(res2$timetotest[3]))))) %>%
  ggplot() +
  stat_ecdf(geom="step",aes(y,colour=delay)) + theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  ggplot2::scale_colour_manual(values = cbPalette[c(2,3,4)],name="test delay (cum. density)") +
  xlim(c(0,15)) +
  geom_point(data=falseNeg, aes(x=Day,y=1-Mean)) +
  geom_linerange(data=falseNeg,aes(x=Day,ymax=1-Lower,ymin=1-Upper)) +
  geom_line(data=falseNeg, aes(x=seq(0,15,length.out=21),y=rep(0.65,21)),linetype=2,col="grey") +
  theme_minimal(base_size = 18) +
  ggplot2::theme(legend.position = "bottom") +
  labs(tag="d",x='time tested (days post-exposure)',y='sensitivity (Kucirka et al.)')

(h1+h3)/(h2+h4)

save(file="data-raw/timetotest_cum_plot.Rdata",h3)

load("data-raw/timetotest_plot.Rdata")
load("data-raw/timetotest_cum_plot.Rdata")
h1+h3

delay4 <- unlist(res1$timetotest[1])
delay2 <- unlist(res1$timetotest[2])
delay0 <- unlist(res1$timetotest[3])

delay4 <- delay4[order(delay4)]
delay2 <- delay2[order(delay2)]
delay0 <- delay0[order(delay0)]

quantile(delay0,0.971)
quantile(delay0,0.645)
quantile(delay0,0.489)
quantile(delay2,0.952)
quantile(delay2,0.293)
quantile(delay2,0.102)
quantile(delay4,0.917)
quantile(delay4,0.001)

# quantiles for 2 day delay:
quants <- c(10.2,29.3,49.6,65.0,74.0,80.7,85.5,89.1,91.8,93.8,95.2,96.3,97.1,97.8,98.2,98.6,98.8,99.0)
quants_shift <- c(0,quants[1:17])
divs <- quants - quants_shift
# sensitivities
sens <- c(0.33,0.62,0.75,0.79,0.8,0.79,0.76,0.74,0.7,0.66,0.61,0.57,0.52,0.48,0.44,0.4,0.37,0.34)
# weighted mean sensitivity for 2 day delay:
sum(divs*sens)/sum(divs)


# ######################
# # Simulation runs:
#
# # Before running: uncomment lines 137, 173 and 174 of outbreak_model.R (timetotest definition)
#
# library(data.table)
# library(tidyverse)
# library(git2r)
# library(tictoc)
# library(ggplot2)
# library(patchwork)
# library(cowplot)
# library(latex2exp)
# library(furrr)
# library(sn)
# library(ggrepel)
# library(testthat)
# library(svglite)
#
# devtools::load_all()
#
# set.seed(200518)
#
# no.samples <- 3000
#
# scenarios <- tidyr::expand_grid(
#   ## Put parameters that are grouped by disease into this data.frame
#   delay_group = list(tibble::tibble(
#     delay = c("Adherence"),
#     delay_shape = c(0.9),
#     delay_scale = 1
#   )),
#   iso_adhere = 1,
#   inc_meanlog = 1.434065,
#   inc_sdlog = 0.6612,
#   inf_shape = 17.773185,
#   inf_rate = 1.388388,
#   inf_shift = 12.978985,
#   min_quar_delay = 1,
#   max_quar_delay = c(1,4),
#   index_R0 = c(1.3),
#   prop.asym = c(0.4),
#   control_effectiveness = c(0.6),
#   self_report = c(0.5),
#   test_delay = c(0,2,4), #time from isolation to test result
#   sensitivity = c(0.65), #percent of cases detected
#   precaution = c(7), #this could be between 0 and 7? Number of days stay in isolation if negative test
#   num.initial.cases = c(5)) %>%
#   tidyr::unnest("delay_group") %>%
#   dplyr::mutate(scenario = 1:dplyr::n())
#
# cap_cases <- 2000
# max_days <- 300
# ## Parameterise fixed paramters
# sim_with_params <- purrr::partial(ringbp::scenario_sim,
#                                   cap_max_days = max_days,
#                                   cap_cases = cap_cases,
#                                   r0isolated = 0,
#                                   disp.iso = 1,
#                                   disp.com = 0.16,
#                                   quarantine = TRUE)
#
# #+ full_run
# tic()
# ## Run parameter sweep
# sweep_results <- ringbp::parameter_sweep(scenarios,
#                                           sim_fn = sim_with_params,
#                                           samples = no.samples,
#                                           show_progress = TRUE)
# toc()
#
#
# # writeout
# saveRDS(sweep_results, file = "data-raw/res_timetotest.rds")



