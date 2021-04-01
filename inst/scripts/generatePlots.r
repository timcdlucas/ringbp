# Loading in results and generating plots

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
library(lemon)


#########################
# Figures 1, 2 and 3

rm(list = ls())
devtools::load_all()
no.samples <- 5000
cap_cases <- 2000
max_days <- 300

# Load in pre-saved results
sweep_results <- readRDS("data-raw/res_Jan_complete_fix.rds")

# A colour-blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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

sweep_results <- sweep_results %>%
  dplyr::group_by(index_R0, sensitivity, test_asym, test_delay, iso_adhere) %>%
  dplyr::mutate(rel_Re = (mean_Re[which(control_effectiveness==0)]-mean_Re)/mean_Re[which(control_effectiveness==0)]) %>%
  dplyr::ungroup()

# Parameter distributions (incubation, generation interval etc.)
ringbp::make_figure_2()

# Figs 1B and S1
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

saveRDS(res,'data-raw/Fig1.rds')
res <- readRDS('data-raw/Fig1.rds')

Fig1 <- res %>%
  filter(test_delay != 0.5) %>%
  filter(test_asym == FALSE) %>%
  filter(sensitivity == 0.95) %>%
  mutate(test_delay = factor(test_delay, labels = c('instant tracing','2 day delay'))) %>%
  mutate(iso_adhere = factor(iso_adhere, labels = c('poor reporting & adherence', 'good reporting & adherence','high adherence'))) %>%
  mutate(index_R0 = factor(index_R0, labels = c("Rs = 1.3","1.5"))) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = test_delay, linetype = test_delay)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2)],name="TTI delay:") +
  ggplot2::scale_linetype_manual(values = c(2,1),name="TTI delay:") +
  geom_line(lwd=1.1) +
  geom_point() +
  geom_linerange(aes(control_effectiveness,ymax=1-lower,ymin=1-upper),show.legend=FALSE) +
  facet_rep_grid(iso_adhere~index_R0,scales='free', repeat.tick.labels = 'all') +
  coord_capped_cart(bottom='both', left='both') +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,3,0.1), 'lines'),legend.key.size = grid::unit(2.5, "lines")) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.6))

Fig2 <- res %>%
  filter(test_delay != 0.5) %>%
  filter(test_asym == FALSE) %>%
  filter(sensitivity == 0.95) %>%
  mutate(test_delay = factor(test_delay, labels = c('instant tracing','2 day delay'))) %>%
  mutate(iso_adhere = factor(iso_adhere, labels = c('poor reporting & adherence', 'good reporting & adherence','high adherence'))) %>%
  mutate(index_R0 = factor(index_R0, labels = c("Rs = 1.3","1.5"))) %>%
  ggplot(aes(control_effectiveness, 100*rel_Re, colour = test_delay, fill = test_delay)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2)],name="TTI delay:") +
  ggplot2::scale_fill_manual(values = cbPalette[c(4,2)],name="TTI delay:") +
  geom_col(aes(y=100*rel_Re),position=position_dodge()) +
  facet_rep_grid(iso_adhere ~ index_R0,scales='free', repeat.tick.labels = 'all') +
  coord_capped_cart(bottom='both', left='both') +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.25,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,3,0.1), 'lines'),legend.key.size = grid::unit(1.5, "lines")) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="% reduction in R") +
  ylim(c(-2,15)) +
  scale_x_continuous(breaks=c(-0.2,0,0.2,0.4,0.6,0.8,1),labels=c("",0,0.2,0.4,0.6,0.8,""), limits = c(-0.2,1))

Fig3 <- res %>%
  filter(test_asym == FALSE) %>%
  filter(iso_adhere == 0.65) %>%
  mutate(test_delay = factor(test_delay, labels = c('instant tracing','1 day delay', '2 day delay'))) %>%
  mutate(index_R0 = factor(index_R0, labels = c("Rs = 1.3","1.5"))) %>%
  mutate(sensitivity = factor(sensitivity)) %>%
  mutate(iso_adhere = factor(iso_adhere,labels=c(""))) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = test_delay, linetype = sensitivity)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name="TTI delay:") +
  ggplot2::scale_linetype_manual(values = c(2,1),name="Test sensitivity:") +
  geom_line(lwd=1.1) +
  geom_point() +
  geom_linerange(aes(control_effectiveness,ymax=1-lower,ymin=1-upper),show.legend=FALSE) +
  facet_rep_grid(iso_adhere ~ index_R0,scales='free', repeat.tick.labels = 'all') +
  coord_capped_cart(bottom='both', left='both') +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.05,-0.17),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,3,0.1), 'lines'),legend.key.size = grid::unit(2.5, "lines")) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.4))


# Manipulate data for further plots

res2 <- list()
week_range <- 40:42

res <- sweep_results

for(i in seq_len(nrow(res))){
  #print(i)
  tmp <- res$sims[i][[1]]
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
    mutate(index_R0 = res$index_R0[i],
           control_effectiveness = res$control_effectiveness[i],
           max_quar_delay = res$max_quar_delay[i],
           iso_adhere = res$iso_adhere[i],
           sensitivity = res$sensitivity[i])

  res2[[i]] <- tmp
}
res2 <- do.call(rbind, res2)

# we want:
# total outbreaks / n
total_cumulative_distr <-
  res2 %>%
  mutate(total = ifelse(total > 2000, 2000, total)) %>%
  group_by(index_R0, control_effectiveness, max_quar_delay, iso_adhere, sensitivity) %>%
  do(res = tibble(cumdistr = nrow(.) * ecdf(.$total)(4:2000),
                  total = 4:2000,
                  outbreaks = nrow(.) - sum(.$extinct),
                  runs = nrow(.),
                  max_quar_delay = .$max_quar_delay[1],
                  index_R0 = .$index_R0[1],
                  iso_adhere = .$iso_adhere[1],
                  sensitivity = .$sensitivity[1],
                  control_effectiveness = .$control_effectiveness[1],
                  poutbreak = pmin(1,(outbreaks) / (runs - cumdistr))))


total_cumulative_distr <- do.call(rbind, total_cumulative_distr$res) %>%
  mutate(index_R0 = factor(index_R0, labels = c('Rs = 1.3','1.5'))) %>%
  mutate(iso_adhere = factor(iso_adhere)) %>%
  mutate(sensitivity = factor(sensitivity, labels = c('65% sensitive','95%'))) %>%
  mutate(max_quar_delay = factor(max_quar_delay, labels = c('instant tracing', '1 day delay', '2 day delay'))) %>%
  filter(outbreaks != 0)

# Fig 3C
T1 <- total_cumulative_distr

lower <- rep(NA,nrow(T1))
upper <- rep(NA,nrow(T1))

for(i in 1:nrow(T1)){

  out <- prop.test(T1$outbreaks[i],max(T1$runs[i]-T1$cumdistr[i],T1$outbreaks[i]),correct=FALSE)
  CIs <- as.numeric(unlist(out[6]))
  lower[i] <- CIs[1]
  upper[i] <- CIs[2]
}

T1 <- T1 %>% mutate(lower = lower,
                    upper = upper)
lower <- c()
upper <- c()

saveRDS(T1,'data-raw/Fig3C.rds')
T1 <- readRDS('data-raw/Fig3C.rds')
T1 <- T1 %>% filter(max_quar_delay=="1 day trace delay") %>%
  mutate(max_quar_delay = " ")

T1 <- T1 %>% mutate(control_effectiveness=factor(control_effectiveness,labels=c("no tracing","20%","40%","60%","80%")))
T1 <- T1 %>% mutate(iso_adhere = factor(iso_adhere, labels = c('poor reporting & adherence', 'average reporting & adherence','average reporting & high adherence')))


Fig3C <- T1 %>%
  filter(sensitivity=="95%") %>%
  filter(max_quar_delay != "1 day delay") %>%
  filter(index_R0=="Rs = 1.3") %>%
  ggplot(aes(total, poutbreak, colour = control_effectiveness)) +
  geom_line(size=1.1,aes(linetype=control_effectiveness)) +
  geom_linerange(alpha=0.1,aes(total,ymax=upper,ymin=lower),show.legend=FALSE) +
  facet_grid(max_quar_delay ~ iso_adhere) +
  scale_colour_manual(values = cbPalette[c(3,8,7,2,4)],name="TTI coverage:") +
  scale_linetype_manual(values = c(1,5,2,4,3),name="TTI coverage:") +
  ylab('Prob. large outbreak') +
  guides(linetype=guide_legend(title="TTI coverage:")) +
  theme_cowplot(font_size = 16) +
  theme(strip.background =element_rect(fill="white")) +
  background_grid() +
  ggplot2::theme(legend.position = c(0.08,0.2), legend.title=element_text(size=14), legend.key.width = unit(2,"cm")) +
  labs(x='Total cases so far',y="Prob. large outbreak") +
  xlim(c(0,1000)) +
  ylim(c(0,1))

save(file="data-raw/outbreakSize_plot.Rdata",Fig3C)


#########################
# Figure 2

rm(list = ls())
devtools::load_all()
no.samples <- 5000
cap_cases <- 2000
max_days <- 300

res <- readRDS("data-raw/res_Aug_perfectTracing.rds")
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

res <- res %>% unnest(trace_stats)

Fig2 <- res %>% filter(cases>=20) %>%
  mutate(precaution = factor(precaution,labels=" ")) %>%
  ggplot(aes(index_R0,positive/cases,fill=index_R0,colour=index_R0)) + geom_boxplot(alpha=0.2) +
  scale_fill_manual(values = cbPalette[c(4,2,7)],name="",guide=FALSE) +
  scale_colour_manual(values = cbPalette[c(4,2,7)],name="",guide=FALSE) +
  ggplot2::labs(x = TeX("Index $\\R_s$"),
                y = 'Proportion cases detected') +
  facet_rep_grid(self_report ~ sensitivity) +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"),axis.line=element_line()) +
  theme(legend.position=c(0.8,0.36),legend.title = element_text(size=14))

save(file="data-raw/Fig2_perfectTraceBox.Rdata",Fig2)

#########################
# Figures 3a and b

rm(list = ls())
devtools::load_all()
no.samples <- 5000
cap_cases <- 2000
max_days <- 300

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

seed.cases <- 5
sweep_results <- readRDS("data-raw/res_Aug_missedChains_5cases.rds")
sims <- rbind(sweep_results$sims[[3]],sweep_results$sims[[1]],sweep_results$sims[[2]])

sims <- sims %>% group_by(index_R0) %>%
  mutate(mid = median(early_missed,na.rm=T), lower = quantile(early_missed,0.05,na.rm=T),
         upper = quantile(early_missed,0.95,na.rm=T)) %>%
  ungroup()

Fig3A <- ggplot(sims, aes(early_missed,group=index_R0,fill=index_R0,colour=index_R0)) +
  geom_density(alpha=0.2) +
  ggplot2::scale_fill_manual(values = cbPalette[c(4,2,7)],guide="none") +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
  guides(color = guide_legend(override.aes = list(fill = cbPalette[c(4,2,7)]))) +
  theme_cowplot(font_size = 16) +
  theme(legend.position=c(0.8,0.85)) +
  labs(tag="a",x='Outbreak size (cases) before first hospitalisation',y="Density") +
  scale_x_continuous(limits=c(5,100),breaks=c(5,25,50,75,100)) +
  geom_vline(xintercept=median(sims$early_missed[which(sims$index_R0==1.1)],na.rm=T),
             linetype=2,colour=cbPalette[4]) +
  geom_vline(xintercept=median(sims$early_missed[which(sims$index_R0==1.3)],na.rm=T),
             linetype=2,colour=cbPalette[2]) +
  geom_vline(xintercept=median(sims$early_missed[which(sims$index_R0==1.5)],na.rm=T),
             linetype=2,colour=cbPalette[7])

seed.cases <- 100
sweep_results <- readRDS("data-raw/res_Aug_missedChains_100cases.rds")
sims <- rbind(sweep_results$sims[[3]],sweep_results$sims[[1]],sweep_results$sims[[2]])

sims <- sims %>% group_by(index_R0) %>%
  mutate(mid = median(early_missed,na.rm=T), lower = quantile(early_missed,0.05,na.rm=T),
         upper = quantile(early_missed,0.95,na.rm=T)) %>%
  ungroup()

Fig3B <- ggplot(sims, aes(early_missed,group=index_R0,fill=index_R0,colour=index_R0)) +
  geom_density(alpha=0.2) +
  ggplot2::scale_fill_manual(values = cbPalette[c(4,2,7)],guide="none") +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name=TeX("Index $\\R_s$")) +
  guides(color = guide_legend(override.aes = list(fill = cbPalette[c(4,2,7)]))) +
  theme_cowplot(font_size = 16) +
  theme(legend.position=c(0.8,0.85)) +
  labs(tag="b",x='Outbreak size (cases) before first hospitalisation',y="") +
  scale_x_continuous(limits=c(100,1000),breaks=c(100,250,500,750,1000)) +
  geom_vline(xintercept=median(sims$early_missed[which(sims$index_R0==1.1)],na.rm=T),
             linetype=2,colour=cbPalette[4]) +
  geom_vline(xintercept=median(sims$early_missed[which(sims$index_R0==1.3)],na.rm=T),
             linetype=2,colour=cbPalette[2]) +
  geom_vline(xintercept=median(sims$early_missed[which(sims$index_R0==1.5)],na.rm=T),
             linetype=2,colour=cbPalette[7])

(Fig3A + Fig3B)/Fig3C

sweep_results$sims[[1]] %>% group_by(sim) %>%
  pos_cumulative

# Plot looking at testing asymptomatics
# FigS1 <- res %>%
#   filter(test_delay==0.5) %>%
#   filter(sensitivity==0.95) %>%
#   filter(index_R0==1.3) %>%
#   mutate(test_asym = factor(test_asym, labels = c('test on symptoms','test all contacts'))) %>%
#   mutate(sensitivity = factor(sensitivity, labels = c('95% sensitvity'))) %>%
#   mutate(iso_adhere = factor(iso_adhere, labels = c('poor reporting & adherence', 'average reporting & adherence','average reporting & high adherence'))) %>%
#   ggplot(aes(control_effectiveness, 1 - pext, colour = test_asym)) +
#   ggplot2::scale_colour_manual(values = cbPalette[c(7,2,4)],name=TeX("TTI strategy:")) +
#   geom_line(lwd=1.1) +
#   geom_point() +
#   geom_linerange(aes(control_effectiveness,ymax=1-lower,ymin=1-upper),show.legend=FALSE) +
#   facet_rep_grid(sensitivity ~ iso_adhere,scales='free', repeat.tick.labels = 'all') +
#   coord_capped_cart(bottom='both', left='both') +
#   theme_cowplot(font_size = 16) +
#   background_grid() +
#   theme(panel.spacing = unit(2, "lines")) +
#   theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
#   ggplot2::theme(legend.position = c(0.3,-0.25),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,3,0.1), 'lines')) +
#   labs(x='Contact tracing coverage',y="Prob. large outbreak") +
#   ylim(c(0,0.4))

# FigS1 <- res %>%
#   filter(test_asym == FALSE) %>%
#   mutate(test_delay = factor(test_delay, labels = c('instant tracing','1 day','2 days'))) %>%
#   mutate(iso_adhere = factor(iso_adhere, labels = c('poor reporting & adherence', 'average reporting & adherence','boosted adherence'))) %>%
#   mutate(sensitivity = factor(sensitivity, labels = c('65%', '95%'))) %>%
#   mutate(index_R0 = factor(index_R0)) %>%
#   ggplot(aes(control_effectiveness, mean_Re, colour = index_R0, linetype = sensitivity)) +
#   ggplot2::scale_colour_manual(values = cbPalette[c(7,2,4)],name=TeX("index $\\R_s$")) +
#   geom_line(lwd=1.1) +
#   geom_point() +
#   geom_linerange(aes(control_effectiveness,ymax=mean_Re+1.96*sd_Re,ymin=mean_Re-1.96*sd_Re),show.legend=FALSE) +
#   facet_rep_grid(test_delay ~ iso_adhere,scales='free', repeat.tick.labels = 'all') +
#   coord_capped_cart(bottom='both', left='both') +
#   theme_cowplot(font_size = 16) +
#   background_grid() +
#   theme(panel.spacing = unit(2, "lines")) +
#   theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
#   ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
#   labs(x='Contact tracing coverage',y="Prob. large outbreak") #+
#   ylim(c(0,0.6))

# #########################
# # Figure 1a
#
# rm(list = ls())
# devtools::load_all()
# no.samples <- 10000
# cap_cases <- 2000
# max_days <- 300
#
# sweep_results <- readRDS("data-raw/res_timetotest.rds")
# falseNeg <- read.csv('data-raw/FalseNegative_kucirka.csv')
#
# # A colour-blind-friendly palette
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#
# res1 <- sweep_results %>%
#   filter(index_R0==1.3) %>%
#   filter(control_effectiveness==0.6) %>%
#   filter(max_quar_delay==1) %>%
#   dplyr::group_by(scenario) %>%
#   dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
#   dplyr::mutate(timetotest = list(unlist(sims[[1]]$timetotest))) %>%
#   dplyr::ungroup(scenario)
#
# res2 <- sweep_results %>%
#   filter(index_R0==1.3) %>%
#   filter(control_effectiveness==0.6) %>%
#   filter(max_quar_delay==4) %>%
#   dplyr::group_by(scenario) %>%
#   dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
#   dplyr::mutate(timetotest = list(unlist(sims[[1]]$timetotest))) %>%
#   dplyr::ungroup(scenario)
#
# df_h1 <- data.frame(y=c(unlist(res1$timetotest[1]),unlist(res1$timetotest[2]),unlist(res1$timetotest[3])),delay=c(rep("4 days",length(unlist(res1$timetotest[1]))),rep("2 days",length(unlist(res1$timetotest[2]))),rep("0 days",length(unlist(res1$timetotest[3])))))
# Fig1A <- df_h1 %>%
#   ggplot() +
#   geom_density(alpha=0.2,aes(y,y=..scaled..,fill=delay,colour=delay)) + theme(text = element_text(size = 16),plot.title = element_text(size = 16, face = "bold")) +
#   ggplot2::scale_colour_manual(values = cbPalette[c(3,8,1)],name="test delay (density)") +
#   ggplot2::scale_fill_manual(values = cbPalette[c(3,8,1)],name="test delay (density)") +
#   xlim(c(0,15)) +
#   geom_point(data=falseNeg, aes(x=Day,y=1-Mean)) +
#   geom_linerange(data=falseNeg,aes(x=Day,ymax=1-Lower,ymin=1-Upper)) +
#   geom_line(data=falseNeg, aes(x=seq(0,15,length.out=21),y=rep(0.65,21)),linetype=2,col="grey") +
#   theme_cowplot(font_size = 16) +
#   ggplot2::theme(legend.position = c(0.8,0.9), legend.title = element_text(size=14)) +
#   labs(tag="a",x='Time tested (days post-exposure)',y='Sensitivity (Kucirka et al.)')
#
# save(file="data-raw/timetotest_plot.Rdata",Fig1A)

#
# Fig1B <- res %>%
#   #filter(self_report == 0.5) %>%
#   #filter(iso_adhere == 0.65) %>%
#   #filter(delay_shape == 0.7) %>%
#   filter(precaution == 7) %>%
#   filter(test_asym == FALSE) %>%
#   filter(sensitivity == c(0.95)) %>%
#   mutate(test_delay = factor(test_delay, labels = c('1 day delay','4 day delay'))) %>%
#   #mutate(precaution = factor(precaution, labels = c('leave quarantine if negative', '7 day quarantine'))) %>%
#   #mutate(test_asym = factor(test_asym, labels = c('test all contacts','symptomatic testing only'))) %>%
#   mutate(self_report = factor(self_report, labels = c('low','high'))) %>%
#   mutate(index_R0 = factor(index_R0, labels = c('Rc = 1.3','1.5'))) %>%
#   mutate(max_quar_delay = factor(max_quar_delay, labels = c('1 day trace delay','4 days'))) %>%
#   ggplot(aes(control_effectiveness, 1 - pext, colour = index_R0, linetype=self_report, shape=self_report)) +
#   ggplot2::scale_colour_manual(values = cbPalette[c(3,8,2)],name="Rc") +
#   ggplot2::scale_linetype_manual(values = c(3,1,2,4),name="adherence") +
#   ggplot2::scale_shape_manual(values = c(15,19,17,18),name="adherence") +
#   geom_line() +
#   geom_point(size=2) +
#   geom_linerange(aes(control_effectiveness,ymax=1-lower,ymin=1-upper),show.legend=FALSE) +
#   facet_rep_grid(test_delay ~ max_quar_delay) +
#   theme_cowplot(font_size = 16) +
#   background_grid() +
#   theme(panel.spacing = unit(2, "lines")) +
#   theme(strip.background =element_rect(fill="white"),axis.line=element_line()) +
#   theme(legend.position=c(0.8,0.36),legend.title = element_text(size=14)) +
#   labs(x='Contact tracing coverage',y="Prob. large outbreak") +
#   ylim(c(0.09,0.45))
#
# save(file="data-raw/sensitivity_plot.Rdata",Fig1B)
# # load("data-raw/sensitivity_plot.Rdata")
#
# Fig1A/Fig1B
#
# save(file="data-raw/precaution_plot.Rdata",FigS1)
# # load("data-raw/precaution_plot.Rdata")
# FigS1
