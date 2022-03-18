library(bbmle)
library(dplyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(gridExtra)
load("rdaout/fit_lognormal_base_comb_rho.rda")

g1 <- ggplot(filter(fit_lognormal_base_rho_comb_nsgtf_within, param=="mean"))  +
  geom_line(aes(rho, est), lwd=1, col="black") +
  geom_line(aes(rho, lwr), lwd=1, col="black") +
  geom_line(aes(rho, upr), lwd=1, col="black") +
  geom_line(data=filter(fit_lognormal_base_rho_comb_sgtf_within, param=="mean", type=="Omicron"), aes(rho, est), lty=2, col="orange", lwd=1) +
  geom_line(data=filter(fit_lognormal_base_rho_comb_sgtf_within, param=="mean", type=="Omicron"), aes(rho, lwr), lty=2, col="orange", lwd=1) +
  geom_line(data=filter(fit_lognormal_base_rho_comb_sgtf_within, param=="mean", type=="Omicron"), aes(rho, upr), lty=2, col="orange", lwd=1) +
  scale_x_continuous("Lognormal correlation coefficient") +
  scale_y_continuous("Mean generation interval (days)") +
  theme(
    panel.grid = element_blank()
  )

ggsave("figure_compare_rho.pdf", g1, width=12, heigh=8)
