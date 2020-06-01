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

seed.cases <- 100

sweep_results <- readRDS("data-raw/res_20200524_missedChains_100cases.rds")

sims <- rbind(sweep_results$sims[[3]],sweep_results$sims[[1]],sweep_results$sims[[2]])

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ggplot(sims, aes(x=early_missed/5,y=index_R0,group=index_R0,colour=index_R0,fill=index_R0)) +
#   geom_boxplot(aes(group=index_R0),alpha=0.2,na.rm=T) +
#   xlim(c(0,12)) +
#   ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
#   ggplot2::scale_fill_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$"),guide="none") +
#   coord_flip() +
#   ylab(TeX("Index $\\R_s$")) +
#   xlab('Scale factor (total cases/initial cases)') +
#   geom_vline(xintercept=2,linetype=2) +
#   theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
#   ggtitle('Outbreak growth by first hospitalisation')

b1 <- ggplot(sims, aes(x=early_missed,y=index_R0,group=index_R0,fill=index_R0,colour=index_R0)) +
  geom_boxplot(aes(group=index_R0),alpha=0.2,na.rm=T) +
  ggplot2::scale_fill_manual(values = cbPalette[c(4,2,7)],guide="none") +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$"),guide="none") +
  coord_flip() +
  ylab(TeX("Index $\\R_s$")) +
  xlab('Total cases') +
  theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  #ggtitle('Outbreak size by first hospitalisation (100 initial cases)') +
  scale_x_log10(limits=c(100,1000),breaks=c(100,200,400,800))

sims <- sims %>% group_by(index_R0) %>%
  mutate(mid = median(early_missed,na.rm=T), lower = quantile(early_missed,0.05,na.rm=T),
         upper = quantile(early_missed,0.95,na.rm=T)) %>%
  ungroup()

g1 <- ggplot(sims, aes(early_missed,group=index_R0,fill=index_R0,colour=index_R0)) +
  geom_density(alpha=0.2) +
  ggplot2::scale_fill_manual(values = cbPalette[c(4,2,7)],guide='none') +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
  theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  xlab('Outbreak size (cases) before first hospitalisation') +
  ylab('Density') +
  #ggtitle('Cases before outbreak observed (hospitalisation)') +
  scale_x_continuous(limits=c(100,1000),breaks=c(100,250,500,750,1000)) +
  geom_vline(xintercept=median(sims$early_missed[which(sims$index_R0==1.1)],na.rm=T),
             linetype=2,colour=cbPalette[4]) +
  geom_vline(xintercept=median(sims$early_missed[which(sims$index_R0==1.3)],na.rm=T),
             linetype=2,colour=cbPalette[2]) +
  geom_vline(xintercept=median(sims$early_missed[which(sims$index_R0==1.5)],na.rm=T),
             linetype=2,colour=cbPalette[7])


g2 <- ggplot(sims, aes(first_iso)) +
  geom_density(alpha=0.2,na.rm=TRUE,colour=cbPalette[1],fill=cbPalette[1],aes(y=..scaled..)) +
  ggplot2::scale_fill_manual(values = cbPalette[c(4,2,7)],guide='none') +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
  theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
  xlab('Days until first hospitalisation') +
  ylab('Density') +
  ggtitle('Time until outbreak observed (1st hospitalisation)')


(b1+g1)


set.seed(200518)
no.samples <- 3000
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
  inf_shape = 2.115779,
  inf_rate = 0.6898583,
  inf_shift = 3,
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

#saveRDS(sweep_results, file = "data-raw/res_20200524_missedChains_100cases.rds")
