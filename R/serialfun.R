library(shellpipes)

kernelfun_lognormal_incubation <- function(y, 
                                           logmean_inc=1,
                                           logsd_inc=0.5) {
  dnorm(log(y), logmean_inc, logsd_inc) * 1/y
}

kernelfun_lognormal_generation <- function(x, y, 
                                           logmean_gen=1,
                                           logsd_gen=0.5,
                                           logmean_inc=1,
                                           logsd_inc=0.5,
                                           rho=0.1) {
  dnorm(log(x), logmean_gen + logsd_gen * rho * (log(y) - logmean_inc)/logsd_inc, sqrt(logsd_gen^2 * (1-rho^2))) * 1/x
}

kernelfun_lognormal <- function(x, y, 
                                logmean_gen=1,
                                logsd_gen=0.5,
                                logmean_inc=1,
                                logsd_inc=0.5,
                                rho=0.1) {
  kernelfun_lognormal_generation(x, y, logmean_gen, logsd_gen, logmean_inc, logsd_inc, rho) * 
    kernelfun_lognormal_incubation(y, logmean_inc, logsd_inc)
}

serialfun_lognormal_internal <- function(x, alpha1, alpha2,
                                         logmean_gen=1,
                                         logsd_gen=0.5,
                                         logmean_inc=1,
                                         logsd_inc=0.5,
                                         rho=0.5,
                                         r=0.1) {
  exp(r * alpha1) * kernelfun_lognormal(alpha2-alpha1, -alpha1,
                                        logmean_gen,
                                        logsd_gen,
                                        logmean_inc,
                                        logsd_inc,
                                        rho) * 
    kernelfun_lognormal_incubation(x-alpha2,
                                   logmean_inc,
                                   logsd_inc)
}

serialfun_lognormal_numer <- function(x,
                                      logmean_gen=1,
                                      logsd_gen=0.5,
                                      logmean_inc=1,
                                      logsd_inc=0.5,
                                      rho=0.5,
                                      r=0.1,
                                      alpha1min=-30,
                                      alphan=0.5e2,
                                      epsilon=1e-3) {
  alpha1vec <- head(seq(alpha1min, min(0, x), length.out=alphan)-epsilon, -1)
  
  ss <- sapply(alpha1vec, function(a1) {
    alpha2vec <- seq(a1+epsilon, x-epsilon, length.out=alphan)
    
    sum(serialfun_lognormal_internal(x, a1, alpha2vec, 
                                          logmean_gen=logmean_gen,
                                          logsd_gen=logsd_gen,
                                          logmean_inc=logmean_inc,
                                          logsd_inc=logsd_inc,
                                          rho=rho,
                                          r=r) * diff(alpha2vec)[1])  
  })
  
  sum(ss * diff(alpha1vec)[1])
}

serialfun_lognormal <- function(x, 
                                logmean_gen=1,
                                logsd_gen=0.5,
                                logmean_inc=1,
                                logsd_inc=0.5,
                                rho=0.5,
                                r=0.1,
                                alpha1min=-30,
                                alphan=0.5e2,
                                epsilon=1e-3,
                                xmin=-10,
                                xmax=20,
                                xby=0.2) {
  xvec <- seq(xmin, xmax, by=xby)
  
  denom_raw <- sapply(xvec, function(y) {
    serialfun_lognormal_numer(y,
                              logmean_gen=logmean_gen,
                              logsd_gen=logsd_gen,
                              logmean_inc=logmean_inc,
                              logsd_inc=logsd_inc,
                              rho=rho,
                              r=r,
                              alpha1min=alpha1min,
                              alphan=alphan,
                              epsilon=epsilon)
  })
  
  denom <- sum(denom_raw*xby)
  
  numer <- denom_raw[match(round(x, 1), round(xvec, 1))]
  
  numer/denom
}

serialfun_lognormal_simulate <- function(nsim=1000, 
                                         logmean_gen=1,
                                         logsd_gen=0.5,
                                         logmean_inc=1,
                                         logsd_inc=0.5,
                                         rho=0.5,
                                         r=0.1) {
  backinc <- rlnorm(nsim, meanlog=logmean_inc, sdlog=logsd_inc)
  
  backinc <- sample(backinc, replace=TRUE, prob=exp(-r*backinc))
  
  logbackinc <- log(backinc)
  
  muloggen <- logmean_gen + logsd_gen * rho * (logbackinc - logmean_inc)/logsd_inc
  sigmaloggen <- sqrt(logsd_gen^2 * (1 - rho^2))
  
  forwardgen <- exp(rnorm(nsim, muloggen, sigmaloggen))
  forwardinc <- rlnorm(nsim, meanlog=logmean_inc, sdlog=logsd_inc)
  
  ser <- -backinc + forwardgen + forwardinc
  
  ser
}

kernelfun_he_incubation <- function(y, 
                                    logmean_inc=1,
                                    logsd_inc=0.5) {
  dlnorm(y, logmean_inc, logsd_inc)
}

kernelfun_he_generation <- function(x, y, 
                                    inf_shape=2,
                                    inf_mean=1,
                                    inf_shift=2) {
  dgamma(x-y+inf_shift, inf_shape, inf_shape/inf_mean)/(1-pgamma(-y+inf_shift, inf_shape, inf_shape/inf_mean))
}

kernelfun_he <- function(x, y, 
                         inf_shape=2,
                         inf_mean=1,
                         inf_shift=2,
                         logmean_inc=1,
                         logsd_inc=0.5) {
  kernelfun_he_generation(x, y, inf_shape, inf_mean, inf_shift) * 
    kernelfun_he_incubation(y, logmean_inc, logsd_inc)
}

serialfun_he_internal <- function(x, alpha1, alpha2,
                                  inf_shape=2,
                                  inf_mean=1,
                                  inf_shift=2,
                                  logmean_inc=1,
                                  logsd_inc=0.5,
                                  r=0.1) {
  exp(r * alpha1) * kernelfun_he(alpha2-alpha1, -alpha1,
                                 inf_shape,
                                 inf_mean,
                                 inf_shift,
                                 logmean_inc,
                                 logsd_inc) * 
    kernelfun_he_incubation(x-alpha2,
                            logmean_inc,
                            logsd_inc)
}

serialfun_he_numer <- function(x,
                               inf_shape=2,
                               inf_mean=1,
                               inf_shift=2,
                               logmean_inc=1,
                               logsd_inc=0.5,
                               r=0.1,
                               alpha1min=-30,
                               alphan=0.5e2,
                               epsilon=1e-3) {
  if (x <= -inf_shift) return(0)
  
  alpha1vec <- head(seq(alpha1min, min(0, x), length.out=alphan)-epsilon, -1)
  
  ss <- sapply(alpha1vec, function(a1) {
    
    alpha2vec <- seq(max(-inf_shift, a1)+epsilon, x-epsilon, length.out=alphan)
    
    sum(serialfun_he_internal(x, a1, alpha2vec, 
                                   inf_shape=inf_shape,
                                   inf_mean=inf_mean,
                                   inf_shift=inf_shift,
                                   logmean_inc=logmean_inc,
                                   logsd_inc=logsd_inc,
                                   r=r) * diff(alpha2vec)[1])  
  })
  
  sum(ss * diff(alpha1vec)[1])
}

serialfun_he <- function(x, 
                         inf_shape=2,
                         inf_mean=1,
                         inf_shift=2,
                         logmean_inc=1,
                         logsd_inc=0.5,
                         r=0.1,
                         alpha1min=-30,
                         alphan=0.5e2,
                         epsilon=1e-3,
                         xmax=20,
                         xby=0.2) {
  xvec <- seq(-inf_shift, xmax, by=xby)
  
  denom_raw <- sapply(xvec, function(y) {
    serialfun_he_numer(y,
                       inf_shape=inf_shape,
                       inf_mean=inf_mean,
                       inf_shift=inf_shift,
                       logmean_inc=logmean_inc,
                       logsd_inc=logsd_inc,
                       r=r,
                       alpha1min=alpha1min,
                       alphan=alphan,
                       epsilon=epsilon)
  })
  
  denom <- sum(denom_raw*xby)
  
  numer <- denom_raw[match(round(x, 1), round(xvec, 1))]
  
  numer/denom
}

saveEnvironment()
