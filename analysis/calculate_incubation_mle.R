source("../R/sample_incubation.R")

r_nsgtf <- -0.05
r_sgtf <- 0.15

moment_0_nsgtf <- integrate(function(z) {
  dweibull(z, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * z)
}, 0, Inf)[[1]]

moment_1_nsgtf <- integrate(function(z) {
  z * dweibull(z, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * z)
}, -100, 100)[[1]]

moment_2_nsgtf <- integrate(function(z) {
  z^2 * dweibull(z, shape=backward_shape_nsgtf, scale=backward_scale_nsgtf) * exp(r_nsgtf * z)
}, -100, 100)[[1]]

moment_0_sgtf <- integrate(function(z) {
  dweibull(z, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * z)
}, 0, Inf)[[1]]

moment_1_sgtf <- integrate(function(z) {
  z * dweibull(z, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * z)
}, -100, 100)[[1]]

moment_2_sgtf <- integrate(function(z) {
  z^2 * dweibull(z, shape=backward_shape_sgtf, scale=backward_scale_sgtf) * exp(r_sgtf * z)
}, -100, 100)[[1]]

mean_inc_nsgtf <- moment_1_nsgtf/moment_0_nsgtf
var_inc_nsgtf <- moment_2_nsgtf/moment_0_nsgtf - mean_inc_nsgtf^2

logsd_inc_nsgtf <- sqrt(log(var_inc_nsgtf/mean_inc_nsgtf^2 + 1))
logmean_inc_nsgtf <- log(mean_inc_nsgtf/exp(logsd_inc_nsgtf^2/2))

mean_inc_sgtf <- moment_1_sgtf/moment_0_sgtf
var_inc_sgtf <- moment_2_sgtf/moment_0_sgtf - mean_inc_sgtf^2

logsd_inc_sgtf <- sqrt(log(var_inc_sgtf/mean_inc_sgtf^2 + 1))
logmean_inc_sgtf <- log(mean_inc_sgtf/exp(logsd_inc_sgtf^2/2))

save("logsd_inc_nsgtf", "logmean_inc_nsgtf", "logsd_inc_sgtf", "logmean_inc_sgtf", file="../rdaout/calculate_incubation_mle.rda")
