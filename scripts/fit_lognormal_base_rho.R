library(bbmle)
library(dplyr)
library(readxl)
source("../R/serialfun.R")
source("../R/fitfun.R")
load("../rdaout/calculate_incubation_mle.rda")
load("../rdaout/fit_lognormal_base.rda")

r_nsgtf <- -0.05
r_sgtf <- 0.15
rhovec <- seq(0.5, 0.9, length.out=11)

serialdata <- read_xlsx("serial-netherlands.xlsx")

serialdata_50_sgtf_within <- serialdata %>%
  filter(week==50, strain=="SGTF", household=="within")

serialdata_50_nsgtf_within <- serialdata %>%
  filter(week==50, strain=="non-SGTF", household=="within")

data_50_sgtf_within <- rep(serialdata_50_sgtf_within$serial, serialdata_50_sgtf_within$n)
data_50_nsgtf_within <- rep(serialdata_50_nsgtf_within$serial, serialdata_50_nsgtf_within$n)

fit_lognormal_base_rho_50_nsgtf_within <- lapply(rhovec, function(rho) {
  ff <- fitfun_lognormal(data=data_50_nsgtf_within, 
                         logmean_gen=coef(fit_lognormal_base_50_nsgtf_within)[[1]], 
                         logsd_gen=coef(fit_lognormal_base_50_nsgtf_within)[[2]], 
                         logmean_inc=logmean_inc_nsgtf, 
                         logsd_inc=logsd_inc_nsgtf, 
                         rho=rho, 
                         r=r_nsgtf)
  
  mean <- exp(coef(ff)[[1]]+coef(ff)[[2]]^2/2)
  
  cc <- confint(ff, method="quad")
  
  tmp <- c(
    exp(coef(ff)[[1]]) * exp(coef(ff)[[2]]^2/2),
    exp(coef(ff)[[1]]) * exp(coef(ff)[[2]]^2/2) * coef(ff)[[2]]
  )
  
  sd_mean <- sqrt(c(tmp %*% vcov(ff) %*% tmp))
  
  data.frame(
    rho=rho,
    param=c("logmean", "logsd", "mean"),
    est=c(coef(ff)[[1]], coef(ff)[[2]], mean),
    lwr=c(cc[1,1], cc[2,1], mean - 1.96 * sd_mean),
    upr=c(cc[1,2], cc[2,2], mean + 1.96 * sd_mean),
    type="Delta"
  )
}) %>%
  bind_rows

fit_lognormal_base_rho_50_sgtf_within <- lapply(rhovec, function(rho) {
  ff <- fitfun_lognormal(data=data_50_sgtf_within, 
                         logmean_gen=coef(fit_lognormal_base_50_sgtf_within)[[1]], 
                         logsd_gen=coef(fit_lognormal_base_50_sgtf_within)[[2]], 
                         logmean_inc=logmean_inc_sgtf, 
                         logsd_inc=logsd_inc_sgtf, 
                         rho=rho, 
                         r=r_sgtf)
  
  mean <- exp(coef(ff)[[1]]+coef(ff)[[2]]^2/2)
  
  cc <- confint(ff, method="quad")
  
  tmp <- c(
    exp(coef(ff)[[1]]) * exp(coef(ff)[[2]]^2/2),
    exp(coef(ff)[[1]]) * exp(coef(ff)[[2]]^2/2) * coef(ff)[[2]]
  )
  
  sd_mean <- sqrt(c(tmp %*% vcov(ff) %*% tmp))
  
  data.frame(
    rho=rho,
    param=c("logmean", "logsd", "mean"),
    est=c(coef(ff)[[1]], coef(ff)[[2]], mean),
    lwr=c(cc[1,1], cc[2,1], mean - 1.96 * sd_mean),
    upr=c(cc[1,2], cc[2,2], mean + 1.96 * sd_mean),
    type="Omicron"
  )
}) %>%
  bind_rows

save("fit_lognormal_base_rho_50_nsgtf_within", "fit_lognormal_base_rho_50_sgtf_within", file="../rdaout/fit_lognormal_base_rho.rda")
