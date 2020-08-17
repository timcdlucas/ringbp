rm(list=ls())
library(fitdistrplus)
library(tidyverse)
library(cowplot)

# inference for infectiousness (results for Fig 1)

#--- data ---
# package: readxl
data = data.frame(readxl::read_xlsx("data-raw/Fig1c_data.xlsx"))
ref.date = as.Date("2020-01-01")
data$x.lb <- as.numeric(as.Date(data$x.lb)-ref.date)
data$x.ub <- as.numeric(as.Date(data$x.ub)-ref.date)
data$y <- as.numeric(as.Date(data$y)-ref.date)
data$SI.lb <- (data$y - data$x.ub) - 0.5
data$SI.ub <- (data$y - data$x.lb) + 0.5

# data: (x.lb, x.ub): lower and upper bounds of infectors symtpom onset dates
# y: symptom onset dates of infectee

#--- incubation period ---
# from Li et al NEJM 2020
# lognormal mean = 5.22; 95% CI = c(4.1, 7.0)
ln.par = c(1.434065, 0.6612)

# Initial values (taken from He et al.)
inf.par <- c()
inf.par[1] <- 2.12
inf.par[2] <- 0.69
inf.par[3] <- 12.3

# Function for calculating log-likelihood
LLfun <- function(inf.par,ln.par,data,N){
  if(inf.par[1] <= 0 | inf.par[2] <=0){ # ensure Gamma parameters are >0
    return(-Inf)
  }
  SIs <- c()
  for(i in 1:N) {
    incubs <- rlnorm(2,ln.par[1],ln.par[2]) # sample infector and infectee incubation periods
    GI <- 0 # set reference generation interval = 0
    while (GI < 0.5) { # require positive generation interval >= 1/2 a day
      inf.time <- rgamma(1,inf.par[1],inf.par[2]) - inf.par[3] # sample infection time (relative to infector symptom onset)
      SI <- inf.time + incubs[2] # serial interval
      GI <- inf.time + incubs[1] # generation interval
    }
    SIs[i] <- SI
  }
  SI.par <- fitdist(SIs+inf.par[3], "lnorm")$estimate # fit a lognormal distribution to empirical SI distribution
  # Likelihood calculations:
  Lik <- plnorm(data$SI.ub + inf.par[3],SI.par[1],SI.par[2]) - plnorm(data$SI.lb + inf.par[3],SI.par[1],SI.par[2])
  LLik <- sum(log(Lik))
  return(LLik)
}

# Maximise LLfun - log-likelihood function
fit <- optim(par=inf.par,
             fn=LLfun,
             ln.par=ln.par,
             data=data,
             N=50000,
             control=list(fnscale=-1))


#####################################################
# Plotting

# A colour-blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ln.par = c(1.434065, 0.6612)

# Gamma distribution parameters for infection time distribution
# shape = 17.773, rate = 1.388, shift = 12.979
# inf.par <- fit$par
inf.par <- c(17.773,1.388,12.979)
inf.par_He <- c(20.516508, 1.592124, 12.272481)
ser.par_He <- c(8.1237578, 0.6361684)
dgamma.shift <- function(x,min.serial,gpar1,gpar2) dgamma(x-min.serial,gpar1,gpar2)
min.serial <- -7

# Calculate empirical SI and GI distributions (as before)
N <- 100000
SIs <- c()
GIs <- c()
for(i in 1:N) {
  incubs <- rlnorm(2,ln.par[1],ln.par[2])
  GI <- 0
  while (GI < 0.5) {
    inf.time <- rgamma(1,inf.par[1],inf.par[2]) - inf.par[3]
    SI <- inf.time + incubs[2]
    GI <- inf.time + incubs[1]
  }
  SIs[i] <- SI
  GIs[i] <- GI
}

# Fit distributions to outputs
SI.ln <- fitdist(SIs+inf.par[3], "lnorm")
SI.par <- SI.ln$estimate
GI.g <- fitdist(GIs, "gamma")
GI.par <- GI.g$estimate

# Serial interval
x1 <- c(seq(0,33,0.1)-inf.par[3],seq(0,33,0.1)+min.serial)
y1 <- c(dlnorm(seq(0,33,0.1),SI.par[1],SI.par[2]),dgamma.shift(seq(-7,26,0.1)+0.5, min.serial, ser.par_He[1], ser.par_He[2]))
ref1 <- c(rep('model fit',331),rep('He et al.',331))
df1 <- tibble(x = x1, y = y1, ref=ref1)
g1 <- ggplot(df1,aes(x=x,y=y,col=ref,linetype=ref)) + geom_line(size=1.5) +
  theme_cowplot(font_size=16) +
  scale_colour_manual(values=cbPalette[c(1,5)],name="") +
  scale_linetype_manual(values=c(3,1),name="") +
  ggplot2::theme(legend.position = c(0.6,0.8), legend.key.width = unit(1,"cm")) +
  labs(tag="c",x="Serial interval (days)",y="") +
  xlim(c(-5,20)) +
  ylim(c(0,0.15))

# Infectiousness
x2 <- c(seq(0,33,0.1)-inf.par[3],seq(0,33,0.1)+-inf.par[3])
y2 <- c(dgamma(seq(0,33,0.1),inf.par[1],inf.par[2]),dgamma(seq(0,33,0.1), inf.par_He[1], inf.par_He[2]))
ref2 <- c(rep('model fit',331),rep('He et al.',331))
df2 <- tibble(x = x2, y = y2, ref=ref2)
g2 <- ggplot(df2,aes(x=x,y=y,col=ref,linetype=ref)) + geom_line(size=1.5) +
  theme_cowplot(font_size=16) +
  scale_colour_manual(values=cbPalette[c(1,5)],name="") +
  scale_linetype_manual(values=c(3,1),name="") +
  ggplot2::theme(legend.position = c(0.6,0.8), legend.key.width = unit(1,"cm")) +
  labs(tag="b",x="Time from symptom onset (days)",y="Density") +
  xlim(c(-10,15)) +
  ylim(c(0,0.15))

GI <- data.frame(gamma_sing = dgamma(seq(0,20,0.1),shape=1.819,scale=2.821),
                 gamma_china = dgamma(seq(0,20,0.1),shape=2.1217,scale=1.028))
GI2 <- data.frame(num=c(GI$gamma_sing,GI$gamma_china),x = c(seq(0,20,0.1),seq(0,20,0.1)),Source=c(rep('Ganyani et al. (1)',201),rep('Ganyani et al. (2)',201)))


# Generation interval
x3 <- rep(seq(0,20,0.1),1)
y3 <- c(dgamma(seq(0,20,0.1),GI.par[1],GI.par[2]))
ref3 <- c(rep("model fit",201))
df3 <- tibble(x = x3, y = y3, ref=ref3)
g3 <- ggplot(df3,aes(x=x,y=y,col=ref,linetype=ref)) + geom_line(size=1.5) +
  theme_cowplot(font_size=16) +
  scale_colour_manual(values=cbPalette[c(5)],name="") +
  scale_linetype_manual(values=c(1),name="") +
  ggplot2::theme(legend.position = c(0.6,0.8), legend.key.width = unit(1,"cm")) +
  labs(tag="d",x="Generation interval (days)",y="") +
  xlim(c(0,20)) +
  ylim(c(0,0.15))

# Incubation period
x4 <- rep(seq(0,20,0.1),1)
y4 <- dgamma(seq(0,20,0.1),ln.par[1],ln.par[2])
ref4 <- rep("Li et al.",201)
df4 <- tibble(x = x4, y = y4, ref=ref4)
g4 <- ggplot(df4,aes(x=x,y=y,col=ref,linetype=ref)) + geom_line(size=1.5) +
  theme_cowplot(font_size=16) +
  scale_colour_manual(values=cbPalette[c(1)],name="") +
  scale_linetype_manual(values=c(1),name="") +
  ggplot2::theme(legend.position = c(0.6,0.8), legend.key.width = unit(1,"cm")) +
  labs(tag="a",x="Time from exposure to onset (days)",y="Density") +
  xlim(c(0,10)) +
  ylim(c(0,0.4))


g4/g2 | g1/g3

