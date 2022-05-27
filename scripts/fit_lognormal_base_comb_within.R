library(bbmle)
source("R/serialfun.R")
source("R/fitfun.R")
source("serialdata.R")
source("baseparam.R")
load("rdaout/calculate_incubation_mle.rda")

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

saveVars("fit_lognormal_base_comb_sgtf_within", "fit_lognormal_base_comb_nsgtf_within")
