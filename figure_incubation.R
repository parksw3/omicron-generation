library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("sample_incubation.R")

rvec_nsgtf <- seq(-0.1, 0, length.out=11)
rvec_sgtf <- seq(0.1, 0.2, length.out=11)

r_nsgtf <- -0.05
r_sgtf <- 0.15

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

g1 <- ggplot(observed_nsgtf) +
  geom_line(aes(time, density, col="Delta", lty="Delta"), lwd=1) +
  geom_line(data=observed_sgtf, aes(time, density, col="Omicron", lty="Omicron"), lwd=1) +
  scale_x_continuous("Observed incubation period (days)", expand=c(0, 0)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.23)) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_linetype_discrete("") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.title = element_blank()
  )

g2 <- ggplot(corrected_nsgtf) +
  geom_line(aes(time, density, col="Delta"), lwd=1) +
  geom_line(data=corrected_sgtf, aes(time, density, col="Omicron"), lwd=1, lty=2) +
  scale_x_continuous("Forward incubation period (days)", expand=c(0, 0), limits=c(0, 14)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.23)) +
  scale_color_manual(values=c("black", "orange")) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g3 <- ggplot(corrected_mean_nsgtf) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_line(aes(r, median), lwd=1) +
  scale_x_continuous("Delta growth rate (1/day)") +
  scale_y_continuous("Mean forward incubation period (days)", limits=c(3.3, 5.6)) +
  theme(
    panel.grid = element_blank()
  )

g4 <- ggplot(corrected_mean_sgtf) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2, fill="orange") + 
  geom_line(aes(r, median), lty=2, col="orange", lwd=1) +
  scale_x_continuous("Omicron growth rate (1/day)") +
  scale_y_continuous("Mean forward incubation period (days)", limits=c(3.3, 5.6)) +
  theme(
    panel.grid = element_blank()
  )

gfinal <- ggarrange(g1, g2, g3, g4, nrow=2, labels=c("A", "B", "C", "D"), draw=FALSE)

ggsave("figure_incubation.pdf", gfinal, width=8, height=6)
