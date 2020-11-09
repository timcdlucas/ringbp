library(tidyverse)
library(cowplot)

res <- readRDS("data-raw/res_Aug_complete.rds")
num_weeks <- 43
num_sims <- 5000

res <- res %>% filter(max_quar_delay=="1")
res <- res %>% filter(test_delay=="2")
res <- res %>% filter(sensitivity=="0.65")
res <- res %>% filter(precaution=="0")

res_low <- res %>% filter(index_R0=="1.1")
res_mid <- res %>% filter(index_R0=="1.3")
res_high <- res %>% filter(index_R0=="1.5")

sims_low <- rbind(res_low$sims[[1]],res_low$sims[[2]],res_low$sims[[3]],res_low$sims[[4]],res_low$sims[[5]])
sims_low <- sims_low %>% mutate(control_effectiveness = c(rep(res_low$control_effectiveness[1],num_weeks*num_sims),
                                          rep(res_low$control_effectiveness[2],num_weeks*num_sims),
                                          rep(res_low$control_effectiveness[3],num_weeks*num_sims),
                                          rep(res_low$control_effectiveness[4],num_weeks*num_sims),
                                          rep(res_low$control_effectiveness[5],num_weeks*num_sims)),
                        index_R0 = rep("1.1",nrow(sims_low)))

sims_mid <- rbind(res_mid$sims[[1]],res_mid$sims[[2]],res_mid$sims[[3]],res_mid$sims[[4]],res_mid$sims[[5]])
sims_mid <- sims_mid %>% mutate(control_effectiveness = c(rep(res_mid$control_effectiveness[1],num_weeks*num_sims),
                                                          rep(res_mid$control_effectiveness[2],num_weeks*num_sims),
                                                          rep(res_mid$control_effectiveness[3],num_weeks*num_sims),
                                                          rep(res_mid$control_effectiveness[4],num_weeks*num_sims),
                                                          rep(res_mid$control_effectiveness[5],num_weeks*num_sims)),
                                index_R0 = rep("1.3",nrow(sims_mid)))

sims_high <- rbind(res_high$sims[[1]],res_high$sims[[2]],res_high$sims[[3]],res_high$sims[[4]],res_high$sims[[5]])
sims_high <- sims_high %>% mutate(control_effectiveness = c(rep(res_high$control_effectiveness[1],num_weeks*num_sims),
                                                          rep(res_high$control_effectiveness[2],num_weeks*num_sims),
                                                          rep(res_high$control_effectiveness[3],num_weeks*num_sims),
                                                          rep(res_high$control_effectiveness[4],num_weeks*num_sims),
                                                          rep(res_high$control_effectiveness[5],num_weeks*num_sims)),
                                index_R0 = rep("1.5",nrow(sims_high)))

sims <- rbind(sims_low,sims_mid,sims_high) %>% mutate(sim = as.factor(sim),
                                                               control_effectiveness = as.factor(control_effectiveness),
                                                               index_R0 = as.factor(index_R0))

sims <- sims %>% group_by(week, control_effectiveness, index_R0) %>%
  mutate(avg = mean(cumulative),
         med = median(cumulative),
         LQ = quantile(cumulative,0.025),
         UQ = quantile(cumulative,0.975)) %>%
  ungroup()

sims <- sims %>% group_by(sim,control_effectiveness,index_R0) %>%
  mutate(extinct = ifelse(max(cumulative)>2000,
                          FALSE,
                          TRUE)) %>%
  ungroup()

sims <- sims %>% group_by(sim, control_effectiveness, index_R0)


sims %>% filter(control_effectiveness == 0 & index_R0 == "1.1") %>%
  ggplot(aes(x=week,y=cumulative,group=sim,color=extinct)) + geom_line() +
  #geom_line(aes(y=UQ)) +
  geom_line(aes(y=avg),lty=2, color='black') +
  #geom_line(aes(y=LQ)) +
  theme_minimal() +
  ylab('new cases (weekly)')

sims %>% filter(control_effectiveness == 0 & index_R0 == "1.3") %>%
  ggplot(aes(x=week,y=cumulative,group=sim,color=extinct)) + geom_line() +
  #geom_line(aes(y=UQ)) +
  geom_line(aes(y=avg),lty=2, color='black') +
  #geom_line(aes(y=LQ)) +
  theme_minimal() +
  ylab('new cases (weekly)')

sims %>% filter(control_effectiveness == 0.8 & index_R0 == "1.5") %>%
  ggplot(aes(x=week,y=cumulative,group=sim,color=extinct)) + geom_line() +
  #geom_line(aes(y=UQ)) +
  geom_line(aes(y=avg),lty=2, color='black') +
  #geom_line(aes(y=LQ)) +
  theme_minimal() +
  ylab('new cases (weekly)')


sims %>% filter(week==42 & control_effectiveness == 0 & index_R0 == "1.1") %>%
  count(extinct==TRUE)
sims %>% filter(week==42 & control_effectiveness == 0.4 & index_R0 == "1.1") %>%
  count(extinct==TRUE)
sims %>% filter(week==42 & control_effectiveness == 0 & index_R0 == "1.1") %>%
  count(extinct==TRUE)

high <- tibble(index_R0 = rep('1.5',6),control_effectiveness = c('0','0','0.4','0.4','0.6','0.6'),
       extinct = rep(c(TRUE,FALSE),3),
       count = c(3737, 1263, 3824, 1176, 3896, 1104))

mid <- tibble(index_R0 = rep('1.3',6),control_effectiveness = c('0','0','0.4','0.4','0.6','0.6'),
       extinct = rep(c(TRUE,FALSE),3),
       count = c(4482, 518, 4565, 435, 4691, 309))

low <- tibble(index_R0 = rep('1.1',6),control_effectiveness = c('0','0','0.4','0.4','0.6','0.6'),
       extinct = rep(c(TRUE,FALSE),3),
       count = c(4976, 24, 4993, 7, 4993, 7))

total <- rbind(low,mid,high)

total <- total %>% mutate(prob = count/(5000))
total <- total %>% group_by(index_R0, extinct) %>%
  mutate(rel_prob = prob/prob[1]) %>%
  ungroup()

total <- total %>% group_by(index_R0, extinct) %>%
  mutate(diff_prob = 100*(prob[1] - prob)) %>%
  ungroup()

total %>% filter(control_effectiveness==0.4) %>% mutate(cases=5)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

g1 <- total %>% filter(extinct==FALSE) %>% ggplot(aes(x=index_R0,y=prob,color=control_effectiveness,fill=control_effectiveness)) +
  geom_col(position="dodge") +
  ggplot2::scale_colour_manual(values = cbPalette[c(1,2,5)],name="proportion traced") +
  ggplot2::scale_fill_manual(values = cbPalette[c(1,2,5)],name="proportion traced") +
  theme_cowplot(font_size = 16) +
  ggplot2::theme(legend.position = c(0.1,0.9), legend.title = element_text(size=14)) +
  labs(tag="a",x="R value under social distancing",y="probability of large epidemic")

g2 <- total %>% filter(extinct==FALSE) %>% ggplot(aes(x=index_R0,y=rel_prob,color=control_effectiveness,fill=control_effectiveness)) +
  geom_col(position="dodge") +
  ggplot2::scale_colour_manual(values = cbPalette[c(1,2,5)],name="proportion traced",guide="none") +
  ggplot2::scale_fill_manual(values = cbPalette[c(1,2,5)],name="proportion traced",guide="none") +
  theme_cowplot(font_size = 16) +
  labs(tag="b",x="R value under social distancing",y="relative probability of large epidemic")

g3 <- total %>% filter(extinct==FALSE & control_effectiveness!="0") %>% ggplot(aes(x=index_R0,y=diff_prob,color=control_effectiveness,fill=control_effectiveness)) +
  geom_col(position="dodge") +
  ggplot2::scale_colour_manual(values = cbPalette[c(2,5)],name="proportion traced",guide="none") +
  ggplot2::scale_fill_manual(values = cbPalette[c(2,5)],name="proportion traced",guide="none") +
  theme_cowplot(font_size = 16) +
  labs(tag="c",x="R value under social distancing",y="number of additional outbreaks averted")

gridExtra::grid.arrange(g1,g2,g3,ncol=3)

sims %>% filter(week==42 & control_effectiveness == 0.6 & index_R0 == "1.1" & cumulative >50) %>%
  count(extinct==TRUE)
sims %>% filter(week==42 & control_effectiveness == 0.4 & index_R0 == "1.1" & cumulative >50) %>%
  count(extinct==TRUE)
sims %>% filter(week==42 & control_effectiveness == 0 & index_R0 == "1.1" & cumulative >50) %>%
  count(extinct==TRUE)

high <- tibble(index_R0 = rep('1.5',6),control_effectiveness = c('0','0','0.4','0.4','0.6','0.6'),
               extinct = rep(c(TRUE,FALSE),3),
               count = c(52, 1263, 62, 1176, 78, 1104),
               total = c(1315,1315,1238,1238,1182,1182))

mid <- tibble(index_R0 = rep('1.3',6),control_effectiveness = c('0','0','0.4','0.4','0.6','0.6'),
              extinct = rep(c(TRUE,FALSE),3),
              count = c(429, 518, 464, 435, 525, 309),
              total = c(947,947,899,899,834,834))

low <- tibble(index_R0 = rep('1.1',6),control_effectiveness = c('0','0','0.4','0.4','0.6','0.6'),
              extinct = rep(c(TRUE,FALSE),3),
              count = c(808, 24, 785, 7, 760, 7),
              total = c(832,832,792,792,767,767))

totaldata <- rbind(low,mid,high)

totaldata <- totaldata %>% mutate(prob = count/total)
totaldata <- totaldata %>% group_by(index_R0, extinct) %>%
  mutate(rel_prob = prob/prob[1]) %>%
  ungroup()

totaldata <- totaldata %>% group_by(index_R0, extinct) %>%
  mutate(diff_prob = 100*(prob[1] - prob)) %>%
  ungroup()

h1 <- totaldata %>% filter(extinct==FALSE) %>% ggplot(aes(x=index_R0,y=prob,color=control_effectiveness,fill=control_effectiveness)) +
  geom_col(position="dodge") +
  ggplot2::scale_colour_manual(values = cbPalette[c(1,2,5)],name="proportion traced") +
  ggplot2::scale_fill_manual(values = cbPalette[c(1,2,5)],name="proportion traced") +
  theme_cowplot(font_size = 16) +
  ggplot2::theme(legend.position = c(0.1,0.9), legend.title = element_text(size=14)) +
  labs(tag="a",x="R value under social distancing",y="probability of large epidemic")

h2 <- totaldata %>% filter(extinct==FALSE) %>% ggplot(aes(x=index_R0,y=rel_prob,color=control_effectiveness,fill=control_effectiveness)) +
  geom_col(position="dodge") +
  ggplot2::scale_colour_manual(values = cbPalette[c(1,2,5)],name="proportion traced",guide="none") +
  ggplot2::scale_fill_manual(values = cbPalette[c(1,2,5)],name="proportion traced",guide="none") +
  theme_cowplot(font_size = 16) +
  labs(tag="b",x="R value under social distancing",y="relative probability of large epidemic")

h3 <- totaldata %>% filter(extinct==FALSE & control_effectiveness!="0") %>% ggplot(aes(x=index_R0,y=diff_prob,color=control_effectiveness,fill=control_effectiveness)) +
  geom_col(position="dodge") +
  ggplot2::scale_colour_manual(values = cbPalette[c(2,5)],name="proportion traced",guide="none") +
  ggplot2::scale_fill_manual(values = cbPalette[c(2,5)],name="proportion traced",guide="none") +
  theme_cowplot(font_size = 16) +
  labs(tag="c",x="R value under social distancing",y="number of additional outbreaks averted")

gridExtra::grid.arrange(h1,h2,h3,ncol=3)
