library(bbmle)
library(dplyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(gridExtra)

library(shellpipes)
rpcall("figure_compare_rho.Rout figure_compare_rho.R advantage.rda rdaout/fit_lognormal_base_comb_rho.rda")

sourceFiles()
loadEnvironments()

g1 <- ggplot(filter(fit_lognormal_base_rho_comb_nsgtf_within, param=="mean"))  +
  geom_ribbon(aes(rho, ymin=lwr, ymax=upr), alpha=0.2) +
  geom_line(aes(rho, est), lwd=1, col="black") +
  geom_ribbon(data=filter(fit_lognormal_base_rho_comb_sgtf_within, param=="mean", type=="Omicron"), aes(rho, ymin=lwr, ymax=upr), fill="orange", alpha=0.2) +
  geom_line(data=filter(fit_lognormal_base_rho_comb_sgtf_within, param=="mean", type=="Omicron"), aes(rho, est), lty=2, col="orange", lwd=1) +
  scale_x_continuous("Lognormal correlation coefficient") +
  scale_y_continuous("Mean generation interval (days)") +
  theme(
    panel.grid = element_blank()
  )

saveGG(g1, width=6, heigh=4)
