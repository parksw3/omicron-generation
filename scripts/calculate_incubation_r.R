library(dplyr)
source("baseparam.R")
source("sample_incubation.R")

corrected_mean_nsgtf <- lapply(1:length(rvec_nsgtf), function(x) {
  numer <- sapply(1:nsample, function(y) {
    integrate(function(z) {
      z * dweibull(z, shape=backward_shape_nsgtf_sample[y], scale=backward_scale_nsgtf_sample[y]) * exp(rvec_nsgtf[x] * z)
    }, 0, Inf)[[1]]
  })
  
  denom <- sapply(1:nsample, function(y) {
    integrate(function(z) {
      dweibull(z, shape=backward_shape_nsgtf_sample[y], scale=backward_scale_nsgtf_sample[y]) * exp(rvec_nsgtf[x] * z)
    }, 0, Inf)[[1]]
  })
  
  mean <- numer/denom
  
  data.frame(
    r=rvec_nsgtf[x],
    median=median(mean),
    lwr=quantile(mean, c(0.025)),
    upr=quantile(mean, c(0.975))
  )
}) %>%
  bind_rows

corrected_mean_sgtf <- lapply(1:length(rvec_sgtf), function(x) {
  numer <- sapply(1:nsample, function(y) {
    integrate(function(z) {
      z * dweibull(z, shape=backward_shape_sgtf_sample[y], scale=backward_scale_sgtf_sample[y]) * exp(rvec_sgtf[x] * z)
    }, 0, 1e3)[[1]]
  })
  
  denom <- sapply(1:nsample, function(y) {
    integrate(function(z) {
      dweibull(z, shape=backward_shape_sgtf_sample[y], scale=backward_scale_sgtf_sample[y]) * exp(rvec_sgtf[x] * z)
    }, 0, 1e3)[[1]]
  })
  
  mean <- numer/denom
  
  data.frame(
    r=rvec_sgtf[x],
    median=median(mean),
    lwr=quantile(mean, c(0.025)),
    upr=quantile(mean, c(0.975))
  )
}) %>%
  bind_rows

observed_nsgtf <- tibble(
  time=seq(0, 14, by=0.1),
  density=dweibull(time, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf)
)

observed_sgtf <- tibble(
  time=seq(0, 14, by=0.1),
  density=dweibull(time, shape=backward_shape_sgtf, scale=backward_scale_sgtf)
)

corrected_nsgtf <- tibble(
  time=seq(0, 28, by=0.1),
  density0=dweibull(time, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * time),
  density=density0/sum(density0*0.1)
)

corrected_sgtf <- tibble(
  time=seq(0, 28, by=0.1),
  density0=dweibull(time, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * time),
  density=density0/sum(density0*0.1)
)

sum(observed_sgtf$time * observed_sgtf$density)/sum(observed_sgtf$density)

sum(observed_nsgtf$time * observed_nsgtf$density)/sum(observed_nsgtf$density)

sum(corrected_sgtf$time * corrected_sgtf$density)/sum(corrected_sgtf$density)

sum(corrected_nsgtf$time * corrected_nsgtf$density)/sum(corrected_nsgtf$density)

save("observed_sgtf", "observed_nsgtf", "corrected_sgtf", "corrected_nsgtf", 
     "corrected_mean_nsgtf", "corrected_mean_sgtf",
     file="../rdaout/calculate_incubation_r.rda")
