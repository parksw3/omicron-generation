library(dplyr)
library(readxl)
source("serialfun.R")
source("fitfun.R")
load("calculate_incubation_mle.rda")

r_nsgtf <- -0.05
r_sgtf <- 0.15
rho <- 0.75

serialdata <- read_xlsx("serial-netherlands.xlsx")

serialdata_50_sgtf_between <- serialdata %>%
  filter(week==50, strain=="SGTF", household=="between")

serialdata_50_nsgtf_between <- serialdata %>%
  filter(week==50, strain=="non-SGTF", household=="between")

data_50_sgtf_between <- rep(serialdata_50_sgtf_between$serial, serialdata_50_sgtf_between$n)
data_50_nsgtf_between <- rep(serialdata_50_nsgtf_between$serial, serialdata_50_nsgtf_between$n)

fit_lognormal_base_50_sgtf_between <- fitfun_lognormal(data=data_50_sgtf_between, 
                                                      logmean_gen=logmean_inc_sgtf, 
                                                      logsd_gen=logsd_inc_sgtf, 
                                                      logmean_inc=logmean_inc_sgtf, 
                                                      logsd_inc=logsd_inc_sgtf, 
                                                      rho=rho, 
                                                      r=r_sgtf)

fit_lognormal_base_50_nsgtf_between <- fitfun_lognormal(data=data_50_nsgtf_between, 
                                                       logmean_gen=logmean_inc_nsgtf, 
                                                       logsd_gen=logsd_inc_nsgtf, 
                                                       logmean_inc=logmean_inc_nsgtf, 
                                                       logsd_inc=logsd_inc_nsgtf, 
                                                       rho=rho, 
                                                       r=r_nsgtf)

save("fit_lognormal_base_50_sgtf_between", "fit_lognormal_base_50_nsgtf_between", file="fit_lognormal_base_50_between.rda")
