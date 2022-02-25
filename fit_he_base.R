library(bbmle)
library(dplyr)
library(readxl)
source("serialfun.R")
source("fitfun.R")
load("calculate_incubation_mle.rda")

r_nsgtf <- -0.05
r_sgtf <- 0.15
rho <- 0.75

serialdata <- read_xlsx("serial-netherlands.xlsx")

serialdata_50_sgtf_within <- serialdata %>%
  filter(week==50, strain=="SGTF", household=="within")

serialdata_50_nsgtf_within <- serialdata %>%
  filter(week==50, strain=="non-SGTF", household=="within")

data_50_sgtf_within <- rep(serialdata_50_sgtf_within$serial, serialdata_50_sgtf_within$n)
data_50_nsgtf_within <- rep(serialdata_50_nsgtf_within$serial, serialdata_50_nsgtf_within$n)

fit_he_base_50_sgtf_within <- fitfun_he(data=data_50_sgtf_within, 
                                        inf_shape=5, 
                                        inf_mean=5, 
                                        inf_shift=5,
                                        logmean_inc=logmean_inc_sgtf, 
                                        logsd_inc=logsd_inc_sgtf, 
                                        r=r_sgtf)

fit_he_base_50_nsgtf_within <- fitfun_he(data=data_50_nsgtf_within, 
                                         inf_shape=5, 
                                         inf_mean=5, 
                                         inf_shift=5,
                                         logmean_inc=logmean_inc_nsgtf, 
                                         logsd_inc=logsd_inc_nsgtf, 
                                         r=r_nsgtf)

save("fit_he_base_50_sgtf_within", "fit_he_base_50_nsgtf_within", file="fit_he_base.rda")
