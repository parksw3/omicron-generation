library(dplyr)
library(readxl)
source("serialfun.R")
source("fitfun.R")
source("sample_incubation.R")

r_nsgtf <- seq(-0.1, 0, length.out=11)
r_sgtf <- seq(0.1, 0.2, length.out=11)

rho <- 0.75

serialdata <- read_xlsx("serial-netherlands.xlsx")

serialdata_50_sgtf_within <- serialdata %>%
  filter(week==50, strain=="SGTF", household=="within")

serialdata_50_nsgtf_within <- serialdata %>%
  filter(week==50, strain=="non-SGTF", household=="within")

data_50_sgtf_within <- rep(serialdata_50_sgtf_within$serial, serialdata_50_sgtf_within$n)
data_50_nsgtf_within <- rep(serialdata_50_nsgtf_within$serial, serialdata_50_nsgtf_within$n)

fit_lognormal_r_50_nsgtf_within <- lapply(r_nsgtf, function(r) {
  print(r)
  moment_0_nsgtf <- integrate(function(z) {
    dweibull(z, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * z)
  }, 0, 1000)[[1]]
  
  moment_1_nsgtf <- integrate(function(z) {
    z * dweibull(z, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * z)
  }, 0, 1000)[[1]]
  
  moment_2_nsgtf <- integrate(function(z) {
    z^2 * dweibull(z, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * z)
  }, 0, 1000)[[1]]
  
  mean_inc_nsgtf <- moment_1_nsgtf/moment_0_nsgtf
  var_inc_nsgtf <- moment_2_nsgtf/moment_0_nsgtf - mean_inc_nsgtf^2
  
  logsd_inc_nsgtf <- sqrt(log(var_inc_nsgtf/mean_inc_nsgtf^2 + 1))
  logmean_inc_nsgtf <- log(mean_inc_nsgtf/exp(logsd_inc_nsgtf^2/2))
  
  ff <- fitfun_lognormal(data=data_50_nsgtf_within, 
                   logmean_gen=logmean_inc_nsgtf, 
                   logsd_gen=logsd_inc_nsgtf, 
                   logmean_inc=logmean_inc_nsgtf, 
                   logsd_inc=logsd_inc_nsgtf, 
                   rho=rho, 
                   r=r)
  
  mean <- exp(coef(ff)[[1]]+coef(ff)[[2]]^2/2)
  
  cc <- confint(ff, method="quad")
  
  tmp <- c(
    exp(coef(ff)[[1]]) * exp(coef(ff)[[2]]^2/2),
    exp(coef(ff)[[1]]) * exp(coef(ff)[[2]]^2/2) * coef(ff)[[2]]
  )
  
  sd_mean <- sqrt(c(tmp %*% vcov(ff) %*% tmp))
  
  data.frame(
    r=r,
    param=c("logmean", "logsd", "mean"),
    est=c(coef(ff)[[1]], coef(ff)[[2]], mean),
    lwr=c(cc[1,1], cc[2,1], mean - 1.96 * sd_mean),
    upr=c(cc[1,2], cc[2,2], mean + 1.96 * sd_mean),
    type="Delta"
  )
}) %>%
  bind_rows

fit_lognormal_r_50_sgtf_within <- lapply(r_sgtf, function(r) {
  print(r)
  moment_0_sgtf <- integrate(function(z) {
    dweibull(z, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * z)
  }, 0, 1000)[[1]]
  
  moment_1_sgtf <- integrate(function(z) {
    z * dweibull(z, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * z)
  }, 0, 1000)[[1]]
  
  moment_2_sgtf <- integrate(function(z) {
    z^2 * dweibull(z, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * z)
  }, 0, 1000)[[1]]
  
  mean_inc_sgtf <- moment_1_sgtf/moment_0_sgtf
  var_inc_sgtf <- moment_2_sgtf/moment_0_sgtf - mean_inc_sgtf^2
  
  logsd_inc_sgtf <- sqrt(log(var_inc_sgtf/mean_inc_sgtf^2 + 1))
  logmean_inc_sgtf <- log(mean_inc_sgtf/exp(logsd_inc_sgtf^2/2))
  
  ff <- fitfun_lognormal(data=data_50_sgtf_within, 
                         logmean_gen=logmean_inc_sgtf, 
                         logsd_gen=logsd_inc_sgtf, 
                         logmean_inc=logmean_inc_sgtf, 
                         logsd_inc=logsd_inc_sgtf, 
                         rho=rho, 
                         r=r)
  
  mean <- exp(coef(ff)[[1]]+coef(ff)[[2]]^2/2)
  
  cc <- confint(ff, method="quad")
  
  tmp <- c(
    exp(coef(ff)[[1]]) * exp(coef(ff)[[2]]^2/2),
    exp(coef(ff)[[1]]) * exp(coef(ff)[[2]]^2/2) * coef(ff)[[2]]
  )
  
  sd_mean <- sqrt(c(tmp %*% vcov(ff) %*% tmp))
  
  data.frame(
    r=r,
    param=c("logmean", "logsd", "mean"),
    est=c(coef(ff)[[1]], coef(ff)[[2]], mean),
    lwr=c(cc[1,1], cc[2,1], mean - 1.96 * sd_mean),
    upr=c(cc[1,2], cc[2,2], mean + 1.96 * sd_mean),
    type="Omicron"
  )
}) %>%
  bind_rows

save("fit_lognormal_r_50_sgtf_within", "fit_lognormal_r_50_nsgtf_within", file="fit_lognormal_r.rda")
