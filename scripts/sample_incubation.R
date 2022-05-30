library(shellpipes)

nsample <- 1000

set.seed(101)
backward_scale_nsgtf <- 4.93
# diff(plnorm(c(4.51, 5.37), meanlog=log(backward_scale_nsgtf), 0.0445))
backward_scale_nsgtf_sample <- rlnorm(nsample, meanlog=log(backward_scale_nsgtf), 0.0445)

backward_shape_nsgtf <- 1.83
# diff(plnorm(c(1.59, 2.08), meanlog=log(backward_shape_nsgtf), 0.068))
backward_shape_nsgtf_sample <- rlnorm(nsample, meanlog=log(backward_shape_nsgtf), 0.068)

backward_scale_sgtf <- 3.60
# diff(plnorm(c(3.23, 3.98), meanlog=log(backward_scale_sgtf), 0.053))
backward_scale_sgtf_sample <- rlnorm(nsample, meanlog=log(backward_scale_sgtf), 0.053)

backward_shape_sgtf <- 1.50
# diff(plnorm(c(1.32, 1.70), meanlog=log(backward_shape_sgtf), 0.0645))
backward_shape_sgtf_sample <- rlnorm(nsample, meanlog=log(backward_shape_sgtf), 0.0645)

saveEnvironment()
