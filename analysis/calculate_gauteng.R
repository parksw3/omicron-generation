load("../rdaout/fit_lognormal_base_comb_within.rda")
r_diff <- 0.32
r_delta <- -0.06
r_omicron <- r_delta + r_diff

gbar_delta_orig <- 6.4
gkappa_delta_orig <- (5.4/6.4)^2

## I have no idea what SD they assume..
## maybe JD or SA might know...
## TODO: ask and confirm
gbar_omicron_orig <- 5.2
gkappa_omicron_orig <- gkappa_delta_orig

R_omicron_orig <- (1 + r_omicron * gbar_omicron_orig * gkappa_omicron_orig)^(1/gkappa_omicron_orig)
R_delta_orig <- (1 + r_delta * gbar_delta_orig * gkappa_delta_orig)^(1/gkappa_delta_orig)

R_omicron_new <- 1/integrate(function(x) {
  dlnorm(x, coef(fit_lognormal_base_comb_sgtf_within)[[1]], coef(fit_lognormal_base_comb_sgtf_within)[[2]]) * exp(-r_omicron * x)
}, 0, 100)[[1]]

R_delta_new <- 1/integrate(function(x) {
  dlnorm(x, coef(fit_lognormal_base_comb_nsgtf_within)[[1]], coef(fit_lognormal_base_comb_nsgtf_within)[[2]]) * exp(-r_delta * x)
}, 0, 100)[[1]]

R_omicron_orig/R_delta_orig

R_omicron_new/R_delta_new
