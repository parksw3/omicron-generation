nsample <- 1000

set.seed(101)
backward_scale_nsgtf <- 4.93
# diff(pgamma(c(4.51, 5.37), shape=500, rate=500/backward_scale_nsgtf))
backward_scale_nsgtf_sample <- rgamma(nsample, shape=500, rate=500/backward_scale_nsgtf)

backward_shape_nsgtf <- 1.83
# diff(pgamma(c(1.59, 2.08), shape=210, rate=210/backward_shape_nsgtf))
backward_shape_nsgtf_sample <- rgamma(nsample, shape=210, rate=210/backward_shape_nsgtf)

backward_scale_sgtf <- 3.60
# diff(pgamma(c(3.23, 3.98), shape=350, rate=350/backward_scale_sgtf))
backward_scale_sgtf_sample <- rgamma(nsample, shape=350, rate=350/backward_scale_sgtf)

backward_shape_sgtf <- 1.50
# diff(pgamma(c(1.32, 1.70), shape=240, rate=240/backward_shape_sgtf))
backward_shape_sgtf_sample <- rgamma(nsample, shape=240, rate=240/backward_shape_sgtf)