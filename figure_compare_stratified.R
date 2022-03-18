library(bbmle)
library(dplyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(gridExtra)
source("serialfun.R")
load("rdaout/fit_lognormal_r.rda")
load("rdaout/fit_lognormal_r_50_between.rda")
load("rdaout/fit_lognormal_r_51_between.rda")
load("rdaout/fit_lognormal_r_51.rda")

g1 <- ggplot(filter(fit_lognormal_r_50_nsgtf_within, param=="mean")) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.5, label="Week 50, within-household") +
  geom_line(aes(r, est), lwd=1) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_x_continuous("Delta growth rate (1/day)") +
  scale_y_continuous("Mean within-household\ngeneration interval (days)", limits=c(2.1, 4.5)) +
  theme(
    panel.grid = element_blank()
  )

g2 <- ggplot(filter(fit_lognormal_r_50_sgtf_within, param=="mean")) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.5, label="Week 50, within-household") +
  geom_line(aes(r, est), lwd=1, col="orange", lty=2) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2, fill="orange") + 
  scale_x_continuous("Omicron growth rate (1/day)") +
  scale_y_continuous("Mean within-household\ngeneration interval (days)", limits=c(2.1, 4.5)) +
  theme(
    panel.grid = element_blank()
  )

g3 <- ggplot(filter(fit_lognormal_r_51_nsgtf_within, param=="mean")) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.5, label="Week 51, within-household") +
  geom_line(aes(r, est), lwd=1) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_x_continuous("Delta growth rate (1/day)") +
  scale_y_continuous("Mean within-household\ngeneration interval (days)", limits=c(2.1, 4.5)) +
  theme(
    panel.grid = element_blank()
  )

g4 <- ggplot(filter(fit_lognormal_r_51_sgtf_within, param=="mean")) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.5, label="Week 51, within-household") +
  geom_line(aes(r, est), lwd=1, col="orange", lty=2) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2, fill="orange") + 
  scale_x_continuous("Omicron growth rate (1/day)") +
  scale_y_continuous("Mean within-household\ngeneration interval (days)", limits=c(2.1, 4.5)) +
  theme(
    panel.grid = element_blank()
  )

g5 <- ggplot(filter(fit_lognormal_r_50_nsgtf_between, param=="mean")) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.5, label="Week 50, between-household") +
  geom_line(aes(r, est), lwd=1) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_x_continuous("Delta growth rate (1/day)") +
  scale_y_continuous("Mean between-household\ngeneration interval (days)", limits=c(2.1, 4.5)) +
  theme(
    panel.grid = element_blank()
  )

g6 <- ggplot(filter(fit_lognormal_r_50_sgtf_between, param=="mean")) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.5, label="Week 50, between-household") +
  geom_line(aes(r, est), lwd=1, col="orange", lty=2) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2, fill="orange") + 
  scale_x_continuous("Omicron growth rate (1/day)") +
  scale_y_continuous("Mean between-household\ngeneration interval (days)", limits=c(2.1, 4.5)) +
  theme(
    panel.grid = element_blank()
  )

g7 <- ggplot(filter(fit_lognormal_r_51_nsgtf_between, param=="mean")) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.5, label="Week 51, between-household") +
  geom_line(aes(r, est), lwd=1) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_x_continuous("Delta growth rate (1/day)") +
  scale_y_continuous("Mean between-household\ngeneration interval (days)", limits=c(2.1, 4.5)) +
  theme(
    panel.grid = element_blank()
  )

g8 <- ggplot(filter(fit_lognormal_r_51_sgtf_between, param=="mean")) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.5, label="Week 51, between-household") +
  geom_line(aes(r, est), lwd=1, col="orange", lty=2) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2, fill="orange") + 
  scale_x_continuous("Omicron growth rate (1/day)") +
  scale_y_continuous("Mean between-household\ngeneration interval (days)", limits=c(2.1, 4.5)) +
  theme(
    panel.grid = element_blank()
  )

gcomb <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, nrow=2, draw=FALSE)

ggsave("figure_compare_stratified.pdf", gcomb, width=12, height=6)
