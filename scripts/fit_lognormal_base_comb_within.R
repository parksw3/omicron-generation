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

serialdata_comb_sgtf_within <- serialdata %>%
  filter(strain=="SGTF", household=="within") %>%
  group_by(serial, strain, household) %>%
  summarize(
    n=sum(n)
  )

serialdata_comb_nsgtf_within <- serialdata %>%
  filter(strain=="non-SGTF", household=="within") %>%
  group_by(serial, strain, household) %>%
  summarize(
    n=sum(n)
  )

data_comb_sgtf_within <- rep(serialdata_comb_sgtf_within$serial, serialdata_comb_sgtf_within$n)
data_comb_nsgtf_within <- rep(serialdata_comb_nsgtf_within$serial, serialdata_comb_nsgtf_within$n)

fit_lognormal_base_comb_sgtf_within <- fitfun_lognormal(data=data_comb_sgtf_within, 
                                                      logmean_gen=logmean_inc_sgtf, 
                                                      logsd_gen=logsd_inc_sgtf, 
                                                      logmean_inc=logmean_inc_sgtf, 
                                                      logsd_inc=logsd_inc_sgtf, 
                                                      rho=rho, 
                                                      r=r_sgtf)

fit_lognormal_base_comb_nsgtf_within <- fitfun_lognormal(data=data_comb_nsgtf_within, 
                                                       logmean_gen=logmean_inc_nsgtf, 
                                                       logsd_gen=logsd_inc_nsgtf, 
                                                       logmean_inc=logmean_inc_nsgtf, 
                                                       logsd_inc=logsd_inc_nsgtf, 
                                                       rho=rho, 
                                                       r=r_nsgtf)

save("fit_lognormal_base_comb_sgtf_within", "fit_lognormal_base_comb_nsgtf_within", file="../rdaout/fit_lognormal_base_comb_within.rda")
