fitfun_lognormal_nll <- function(logmean_gen=1,
                                 logsd_gen=0.5,
                                 logmean_inc=1,
                                 logsd_inc=0.5,
                                 rho=0.5,
                                 r=0.1,
                                 data,
                                 debug,
                                 alphan=0.5e2) {
  if (logsd_gen < 0 || logmean_gen < 0 ) {
    return(Inf)
  }
  
  ss <- try(serialfun_lognormal(data, logmean_gen=logmean_gen, logsd_gen=logsd_gen, 
                            logmean_inc=logmean_inc, 
                            logsd_inc=logsd_inc, rho=rho, r=r, alphan=alphan))
  
  if (inherits(ss, "try-error")) return(Inf)
  
  if (debug) print(-sum(log(ss)))
  
  -sum(log(ss))  
}

fitfun_lognormal <- function(data,
                             logmean_gen,
                             logsd_gen,
                             logmean_inc,
                             logsd_inc,
                             rho,
                             r,
                             debug=FALSE,
                             alphan=0.5e2) {
  mm <- mle2(fitfun_lognormal_nll, 
       start=list(logmean_gen=logmean_gen, logsd_gen=logsd_gen),
       data=list(data=data, debug=debug, alphan=alphan),
       fixed=list(logmean_inc=logmean_inc, logsd_inc=logsd_inc, rho=rho, r=r))
  
}

fitfun_he_nll <- function(inf_shape=2,
                          inf_mean=1,
                          inf_shift=2,
                          logmean_inc=1,
                          logsd_inc=0.5,
                          r=0.1,
                          data,
                          debug) {
  
  
  ss <- serialfun_he(data, inf_shape, inf_mean, inf_shift, logmean_inc, logsd_inc, r)
  
  if (debug) print(-sum(log(ss)))
  
  -sum(log(ss))  
}

fitfun_he <- function(data,
                      inf_shape,
                      inf_mean,
                      inf_shift,
                      logmean_inc,
                      logsd_inc,
                      r,
                      debug=FALSE) {
  mm <- mle2(fitfun_he_nll, 
             start=list(inf_shape=inf_shape, inf_mean=inf_mean),
             data=list(data=data, debug=debug),
             fixed=list(logmean_inc=logmean_inc, logsd_inc=logsd_inc, inf_shift=inf_shift, r=r))
}
