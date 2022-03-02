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
  geom_line(aes(time, density, col="Delta"), lwd=1) +
  geom_line(data=observed_sgtf, aes(time, density, col="Omicron"), lwd=1) +
  scale_x_continuous("Observed incubation period (days)", expand=c(0, 0)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.23)) +
  scale_color_manual(values=c("black", "orange")) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.title = element_blank()
  )

g2 <- ggplot(corrected_nsgtf) +
  geom_line(aes(time, density, col="Delta"), lwd=1) +
  geom_line(data=corrected_sgtf, aes(time, density, col="Omicron"), lwd=1) +
  scale_x_continuous("Forward incubation period (days)", expand=c(0, 0), limits=c(0, 14)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.23)) +
  scale_color_manual(values=c("black", "orange")) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g3 <- ggplot(corrected_mean_nsgtf) +
  geom_line(aes(r, median), lwd=1) +
  geom_line(aes(r, lwr))  +
  geom_line(aes(r, upr)) +
  geom_line(data=corrected_mean_sgtf, aes(r-0.2, median), lty=2, col="orange", lwd=1)  +
  geom_line(data=corrected_mean_sgtf, aes(r-0.2, lwr), lty=2, col="orange")  +
  geom_line(data=corrected_mean_sgtf, aes(r-0.2, upr), lty=2, col="orange") +
  scale_x_continuous("Delta growth rate (1/day)",
                     sec.axis = sec_axis(trans=~.+0.2, "Omicron growth rate (1/day)")) +
  scale_y_continuous("Mean forward incubation period (days)") +
  theme(
    panel.grid = element_blank(),
    axis.line.x.top = element_line(color="orange"),
    axis.ticks.x.top = element_line(color="orange"),
    axis.text.x.top = element_text(color="orange"),
    axis.title.x.top = element_text(color="orange")
  )

gfinal <- ggarrange(g1, g2, g3, nrow=1, labels=c("A", "B", "C"), draw=FALSE)

ggsave("figure_incubation.pdf", gfinal, width=12, height=4)
