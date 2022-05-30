library(bbmle)
library(dplyr)

library(shellpipes)
loadEnvironments()

objects()
* fit_lognormal_base_rho_comb_nsgtf_within <- lapply(rhovec, function(rho) {
  print(rho)
  ff <- fitfun_lognormal(data=data_comb_nsgtf_within, 
                         logmean_gen=coef(fit_lognormal_base_comb_nsgtf_within)[[1]], 
                         logsd_gen=coef(fit_lognormal_base_comb_nsgtf_within)[[2]], 
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

fit_lognormal_base_rho_comb_sgtf_within <- lapply(rhovec, function(rho) {
  print(rho)
  ff <- fitfun_lognormal(data=data_comb_sgtf_within, 
                         logmean_gen=coef(fit_lognormal_base_comb_sgtf_within)[[1]], 
                         logsd_gen=coef(fit_lognormal_base_comb_sgtf_within)[[2]], 
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

saveVars("fit_lognormal_base_rho_comb_nsgtf_within", "fit_lognormal_base_rho_comb_sgtf_within")
