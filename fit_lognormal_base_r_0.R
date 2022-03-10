library(dplyr)
library(readxl)
source("serialfun.R")
source("fitfun.R")
source("sample_incubation.R")

r_nsgtf <- 0
r_sgtf <- 0
rho <- 0.75

moment_0_nsgtf <- integrate(function(z) {
  dweibull(z, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * z)
}, 0, Inf)[[1]]

moment_1_nsgtf <- integrate(function(z) {
  z * dweibull(z, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * z)
}, -100, 100)[[1]]

moment_2_nsgtf <- integrate(function(z) {
  z^2 * dweibull(z, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * z)
}, -100, 100)[[1]]

moment_0_sgtf <- integrate(function(z) {
  dweibull(z, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * z)
}, 0, Inf)[[1]]

moment_1_sgtf <- integrate(function(z) {
  z * dweibull(z, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * z)
}, -100, 100)[[1]]

moment_2_sgtf <- integrate(function(z) {
  z^2 * dweibull(z, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * z)
}, -100, 100)[[1]]

mean_inc_nsgtf <- moment_1_nsgtf/moment_0_nsgtf
var_inc_nsgtf <- moment_2_nsgtf/moment_0_nsgtf - mean_inc_nsgtf^2

logsd_inc_nsgtf <- sqrt(log(var_inc_nsgtf/mean_inc_nsgtf^2 + 1))
logmean_inc_nsgtf <- log(mean_inc_nsgtf/exp(logsd_inc_nsgtf^2/2))

mean_inc_sgtf <- moment_1_sgtf/moment_0_sgtf
var_inc_sgtf <- moment_2_sgtf/moment_0_sgtf - mean_inc_sgtf^2

logsd_inc_sgtf <- sqrt(log(var_inc_sgtf/mean_inc_sgtf^2 + 1))
logmean_inc_sgtf <- log(mean_inc_sgtf/exp(logsd_inc_sgtf^2/2))

serialdata <- read_xlsx("serial-netherlands.xlsx")

serialdata_50_sgtf_within <- serialdata %>%
  filter(week==50, strain=="SGTF", household=="within")

serialdata_50_nsgtf_within <- serialdata %>%
  filter(week==50, strain=="non-SGTF", household=="within")

data_50_sgtf_within <- rep(serialdata_50_sgtf_within$serial, serialdata_50_sgtf_within$n)
data_50_nsgtf_within <- rep(serialdata_50_nsgtf_within$serial, serialdata_50_nsgtf_within$n)

fit_lognormal_base_r_0_50_sgtf_within <- fitfun_lognormal(data=data_50_sgtf_within, 
                                                      logmean_gen=logmean_inc_sgtf, 
                                                      logsd_gen=logsd_inc_sgtf, 
                                                      logmean_inc=logmean_inc_sgtf, 
                                                      logsd_inc=logsd_inc_sgtf, 
                                                      rho=rho, 
                                                      r=r_sgtf)

fit_lognormal_base_r_0_50_nsgtf_within <- fitfun_lognormal(data=data_50_nsgtf_within, 
                                                       logmean_gen=logmean_inc_nsgtf, 
                                                       logsd_gen=logsd_inc_nsgtf, 
                                                       logmean_inc=logmean_inc_nsgtf, 
                                                       logsd_inc=logsd_inc_nsgtf, 
                                                       rho=rho, 
                                                       r=r_nsgtf)

save("fit_lognormal_base_r_0_50_sgtf_within", "fit_lognormal_base_r_0_50_nsgtf_within", file="fit_lognormal_base_r_0.rda")
