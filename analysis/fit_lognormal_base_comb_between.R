library(bbmle)
library(dplyr)
library(readxl)
source("../R/serialfun.R")
source("../R/fitfun.R")
load("../rdaout/calculate_incubation_mle.rda")

r_nsgtf <- -0.05
r_sgtf <- 0.15
rho <- 0.75

serialdata <- read_xlsx("serial-netherlands.xlsx")

serialdata_comb_sgtf_between <- serialdata %>%
  filter(strain=="SGTF", household=="between") %>%
  group_by(serial, strain, household) %>%
  summarize(
    n=sum(n)
  )

serialdata_comb_nsgtf_between <- serialdata %>%
  filter(strain=="non-SGTF", household=="between") %>%
  group_by(serial, strain, household) %>%
  summarize(
    n=sum(n)
  )

data_comb_sgtf_between <- rep(serialdata_comb_sgtf_between$serial, serialdata_comb_sgtf_between$n)
data_comb_nsgtf_between <- rep(serialdata_comb_nsgtf_between$serial, serialdata_comb_nsgtf_between$n)

fit_lognormal_base_comb_sgtf_between <- fitfun_lognormal(data=data_comb_sgtf_between, 
                                                      logmean_gen=logmean_inc_sgtf, 
                                                      logsd_gen=logsd_inc_sgtf, 
                                                      logmean_inc=logmean_inc_sgtf, 
                                                      logsd_inc=logsd_inc_sgtf, 
                                                      rho=rho, 
                                                      r=r_sgtf)

fit_lognormal_base_comb_nsgtf_between <- fitfun_lognormal(data=data_comb_nsgtf_between, 
                                                       logmean_gen=logmean_inc_nsgtf, 
                                                       logsd_gen=logsd_inc_nsgtf, 
                                                       logmean_inc=logmean_inc_nsgtf, 
                                                       logsd_inc=logsd_inc_nsgtf, 
                                                       rho=rho, 
                                                       r=r_nsgtf)

save("fit_lognormal_base_comb_sgtf_between", "fit_lognormal_base_comb_nsgtf_between", file="../rdaout/fit_lognormal_base_comb_between.rda")
