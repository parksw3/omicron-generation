library(bbmle)

library(shellpipes)
loadEnvironments()

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

saveVars("fit_lognormal_base_50_sgtf_between", "fit_lognormal_base_50_nsgtf_between")
