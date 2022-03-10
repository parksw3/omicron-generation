library(bbmle)
library(dplyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(gridExtra)
source("serialfun.R")
load("rdaout/fit_lognormal_base_r_0.rda")

mean_nsgtf <- exp(coef(fit_lognormal_base_r_0_50_nsgtf_within)[[1]]+coef(fit_lognormal_base_r_0_50_nsgtf_within)[[2]]^2/2)

tmp_nsgtf <- c(
  exp(coef(fit_lognormal_base_r_0_50_nsgtf_within)[[1]]) * exp(coef(fit_lognormal_base_r_0_50_nsgtf_within)[[2]]^2/2),
  exp(coef(fit_lognormal_base_r_0_50_nsgtf_within)[[1]]) * exp(coef(fit_lognormal_base_r_0_50_nsgtf_within)[[2]]^2/2) * coef(fit_lognormal_base_r_0_50_nsgtf_within)[[2]]
)

sd_mean_nsgtf <- sqrt(c(tmp_nsgtf %*% vcov(fit_lognormal_base_r_0_50_nsgtf_within) %*% tmp_nsgtf))

mean_sgtf <- exp(coef(fit_lognormal_base_r_0_50_sgtf_within)[[1]]+coef(fit_lognormal_base_r_0_50_sgtf_within)[[2]]^2/2)

tmp_sgtf <- c(
  exp(coef(fit_lognormal_base_r_0_50_sgtf_within)[[1]]) * exp(coef(fit_lognormal_base_r_0_50_sgtf_within)[[2]]^2/2),
  exp(coef(fit_lognormal_base_r_0_50_sgtf_within)[[1]]) * exp(coef(fit_lognormal_base_r_0_50_sgtf_within)[[2]]^2/2) * coef(fit_lognormal_base_r_0_50_sgtf_within)[[2]]
)

sd_mean_sgtf <- sqrt(c(tmp_sgtf %*% vcov(fit_lognormal_base_r_0_50_sgtf_within) %*% tmp_sgtf))

c(mean_nsgtf - 1.96 * sd_mean_nsgtf, mean_nsgtf, mean_nsgtf + 1.96 * sd_mean_nsgtf)
c(mean_sgtf - 1.96 * sd_mean_sgtf, mean_sgtf, mean_sgtf + 1.96 * sd_mean_sgtf)

serialdata <- read_xlsx("serial-netherlands.xlsx")

xvec <- seq(-6, 16, by=0.2)

ivec <- seq(0, 16, by=0.2)
gvec <- seq(0, 16, by=0.2)

serialdata_50_sgtf_within <- serialdata %>%
  filter(week==50, strain=="SGTF", household=="within")

serialdata_50_nsgtf_within <- serialdata %>%
  filter(week==50, strain=="non-SGTF", household=="within")

serial_density_lognormal_nsgtf <- data.frame(
  x=xvec,
  y=do.call(serialfun_lognormal, c(list(x=xvec), as.list(coef(fit_lognormal_base_r_0_50_nsgtf_within)))),
  type="Delta"
)

serial_density_lognormal_sgtf <- data.frame(
  x=xvec,
  y=do.call(serialfun_lognormal, c(list(x=xvec), as.list(coef(fit_lognormal_base_r_0_50_sgtf_within)))),
  type="Omicron"
)

serial_density_lognormal <- bind_rows(
  serial_density_lognormal_nsgtf,
  serial_density_lognormal_sgtf
)

kernel_lognormal_nsgtf <- data.frame(
  gen=rep(gvec, each=length(ivec)),
  inc=rep(ivec, length(gvec)),
  z=c(sapply(gvec, function(x) {
    do.call(kernelfun_lognormal, c(list(x=x, y=ivec), as.list(coef(fit_lognormal_base_r_0_50_nsgtf_within)[-6])))
  }))
) %>%
  mutate(
    z=ifelse(is.finite(z), z, 0)
  )

kernel_lognormal_sgtf <- data.frame(
  gen=rep(gvec, each=length(ivec)),
  inc=rep(ivec, length(gvec)),
  z=c(sapply(gvec, function(x) {
    do.call(kernelfun_lognormal, c(list(x=x, y=ivec), as.list(coef(fit_lognormal_base_r_0_50_sgtf_within)[-6])))
  }))
) %>%
  mutate(
    z=ifelse(is.finite(z), z, 0)
  )

g1 <- ggplot(serialdata_50_nsgtf_within) +
  geom_point(aes(serial-0.1, n/sum(n), col="Delta", shape="Delta"), size=3) +
  geom_point(data=serialdata_50_sgtf_within, aes(serial+0.1, n/sum(n), col="Omicron", shape="Omicron"), size=3) +
  geom_line(data=serial_density_lognormal_nsgtf, aes(x, y, col="Delta", lty="Delta"), lwd=1) +
  geom_line(data=serial_density_lognormal_sgtf, aes(x, y, col="Omicron", lty="Omicron"), lwd=1) +
  scale_x_continuous("Forward serial intervals (days)", expand=c(0, 0), limits=c(-6, 16)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.22)) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_linetype_discrete("") +
  scale_shape_discrete("") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.85),
    legend.background = element_rect(fill=NA)
  )

genden_nsgtf <- tibble(
  time=seq(0, 14, by=0.1),
  density=dlnorm(time, coef(fit_lognormal_base_r_0_50_nsgtf_within)[[1]], coef(fit_lognormal_base_r_0_50_nsgtf_within)[[2]])
)

genden_sgtf <- tibble(
  time=seq(0, 14, by=0.1),
  density=dlnorm(time, coef(fit_lognormal_base_r_0_50_sgtf_within)[[1]], coef(fit_lognormal_base_r_0_50_sgtf_within)[[2]])
)

g2 <- ggplot(genden_nsgtf) +
  geom_line(aes(time, density), lwd=1) +
  geom_line(data=genden_sgtf, aes(time, density), lwd=1, lty=2, col="Orange") +
  scale_x_continuous("Forward generation intervals (days)", expand=c(0, 0), limits=c(0, 14)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.35)) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_linetype_discrete("") +
  scale_shape_discrete("") +
  theme(
    panel.grid = element_blank()
  )



gcomb1 <- ggarrange(g1, g2, nrow=1, draw=FALSE, 
                    labels=c("A", "B"))

ggsave("figure_compare_r_0.pdf", gcomb1, width=8, heigh=4)
