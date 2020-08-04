rm(list=ls())

# inference for infectiousness (results for Fig 1)

#--- data ---
# package: readxl
data = data.frame(readxl::read_xlsx("Fig1c_data.xlsx"))
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

# Gamma distribution parameters for infection time distribution
# shape = 17.773, rate = 1.388, shift = 12.979
inf.par <- fit$par

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
x1 <- seq(0,33,0.1)-inf.par[3]
y1 <- dlnorm(seq(0,33,0.1),SI.par[1],SI.par[2])
df1 <- tibble(x = x1, y = y1)
g1 <- ggplot(df1,aes(x=x,y=y)) + geom_line(col=cbPalette[2],size=1.5) + geom_area(fill=cbPalette[2],alpha=0.5) +
  theme_cowplot() +
  xlab("serial interval (days)") +
  ylab("density") +
  xlim(c(-5,20)) +
  ylim(c(0,0.15))


# Generation interval
x2 <- seq(0,20,0.1)
y2 <- dgamma(seq(0,20,0.1),GI.par[1],GI.par[2])
df2 <- tibble(x = x2, y = y2)
g2 <- ggplot(df2,aes(x=x,y=y)) + geom_line(col=cbPalette[3],size=1.5) + geom_area(fill=cbPalette[3],alpha=0.5) +
  theme_cowplot() +
  xlab("generation interval (days)") +
  ylab("density")+
  xlim(c(0,20)) +
  ylim(c(0,0.15))

g1/g2
