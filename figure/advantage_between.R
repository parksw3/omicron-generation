library(bbmle)
library(tidyr)
library(dplyr)
library(mvtnorm)
library(shellpipes)
rpcall("advantage.Rout advantage.R cases.rda rdaout/fit_lognormal_base_comb_between.rda")

sourceFiles()
loadEnvironments()

set.seed(101)
lognormal_sim1 <- rmvnorm(nsample, mean=coef(fit_lognormal_base_comb_nsgtf_between)[1:2], sigma=vcov(fit_lognormal_base_comb_nsgtf_between))
lognormal_sim2 <- rmvnorm(nsample, mean=coef(fit_lognormal_base_comb_sgtf_between)[1:2], sigma=vcov(fit_lognormal_base_comb_sgtf_between))

advantagedata <- lapply(1:nsample, function(x) {
  i1 <- c(exp(gfit1_p1 %*% c(gfit1_sim[x,]))/7)
  i2 <- c(exp(gfit2_p1 %*% c(gfit2_sim[x,]))/7)
  
  gen1 <- diff(plnorm(seq(0, 14, by=0.5), lognormal_sim1[x,1], lognormal_sim1[x,2]))
  gen2 <- diff(plnorm(seq(0, 14, by=0.5), lognormal_sim2[x,1], lognormal_sim2[x,2]))
  
  n <- length(gen1)
  
  R_delta <- tail(i1, -n)/sapply(1:(length(i1)-n), function(y) sum(i1[y:(y+n-1)] * rev(gen1)))
  R_omicron <- tail(i2, -n)/sapply(1:(length(i2)-n), function(y) sum(i2[y:(y+n-1)] * rev(gen2)))
  R_omicron_naive <- tail(i2, -n)/sapply(1:(length(i2)-n), function(y) sum(i2[y:(y+n-1)] * rev(gen1)))
  
  data.frame(
    R_delta=R_delta,
    R_omicron=R_omicron,
    R_omicron_naive=R_omicron_naive,
    R_advantage=R_omicron/R_delta,
    R_advantage_naive=R_omicron_naive/R_delta,
    r_delta=tail(gfit1_post[,x], -n),
    r_omicron=tail(gfit2_post[,x], -n),
    r_advantage=tail(advantage_post[,x], -n),
    sim=x,
    time=tail(seq(1, 10, by=1/14), -n)
  )
}) %>%
  bind_rows

advantagesumm <- advantagedata %>%
  gather(key, value, -time, -sim) %>%
  group_by(time, key) %>%
  summarize(
    median=median(value),
    lwr=quantile(value, 0.025),
    upr=quantile(value, 0.975)
  )

advantagesumm2 <- advantagesumm %>%
  select(-lwr, -upr) %>%
  spread(key, median)

saveEnvironment()
