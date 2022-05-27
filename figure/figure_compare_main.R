library(bbmle)
library(dplyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=14))
library(egg)
library(gridExtra)
library(shellpipes)

loadEnvironments()

filter(fit_lognormal_r_comb_nsgtf_within, param=="mean", r==-0.05)
filter(fit_lognormal_r_comb_sgtf_within, param=="mean", round(r, 2)==0.15)

filter(fit_lognormal_r_comb_nsgtf_between, param=="mean", r==-0.05)
filter(fit_lognormal_r_comb_sgtf_between, param=="mean", round(r, 2)==0.15)

xmin <- -6
xmax <- 16
sstep <- 0.2
xvec <- seq(xmin, xmax, by=sstep)

# t.test(rep(serialdata_comb_sgtf_within$serial, serialdata_comb_sgtf_within$n))

# t.test(rep(serialdata_comb_nsgtf_within$serial, serialdata_comb_nsgtf_within$n))

# t.test(rep(serialdata_comb_sgtf_between$serial, serialdata_comb_sgtf_between$n))

# t.test(rep(serialdata_comb_nsgtf_between$serial, serialdata_comb_nsgtf_between$n))

serial_density_lognormal_nsgtf_within <- data.frame(
  x=xvec,
  y=do.call(serialfun_lognormal, c(list(x=xvec), as.list(coef(fit_lognormal_base_comb_nsgtf_within)))),
  type="Delta"
)

serial_density_lognormal_sgtf_within <- data.frame(
  x=xvec,
  y=do.call(serialfun_lognormal, c(list(x=xvec), as.list(coef(fit_lognormal_base_comb_sgtf_within)))),
  type="Omicron"
)

serial_density_lognormal_nsgtf_between <- data.frame(
  x=xvec,
  y=do.call(serialfun_lognormal, c(list(x=xvec), as.list(coef(fit_lognormal_base_comb_nsgtf_between)))),
  type="Delta"
)

serial_density_lognormal_sgtf_between <- data.frame(
  x=xvec,
  y=do.call(serialfun_lognormal, c(list(x=xvec), as.list(coef(fit_lognormal_base_comb_sgtf_between)))),
  type="Omicron"
)

serial_density_lognormal_within <- bind_rows(
  serial_density_lognormal_nsgtf_within,
  serial_density_lognormal_sgtf_within
)

serial_density_lognormal_between <- bind_rows(
  serial_density_lognormal_nsgtf_between,
  serial_density_lognormal_sgtf_between
)

genden_nsgtf_within <- tibble(
  time=seq(0, 14, by=0.1),
  density=dlnorm(time, coef(fit_lognormal_base_comb_nsgtf_within)[[1]], coef(fit_lognormal_base_comb_nsgtf_within)[[2]])
)

genden_sgtf_within <- tibble(
  time=seq(0, 14, by=0.1),
  density=dlnorm(time, coef(fit_lognormal_base_comb_sgtf_within)[[1]], coef(fit_lognormal_base_comb_sgtf_within)[[2]])
)

genden_nsgtf_between <- tibble(
  time=seq(0, 14, by=0.1),
  density=dlnorm(time, coef(fit_lognormal_base_comb_nsgtf_between)[[1]], coef(fit_lognormal_base_comb_nsgtf_between)[[2]])
)

genden_sgtf_between <- tibble(
  time=seq(0, 14, by=0.1),
  density=dlnorm(time, coef(fit_lognormal_base_comb_sgtf_between)[[1]], coef(fit_lognormal_base_comb_sgtf_between)[[2]])
)

g1 <- ggplot(serialdata_comb_nsgtf_within) +
  geom_point(aes(serial-0.1, n/sum(n), col="Delta", shape="Delta"), size=3) +
  geom_point(data=serialdata_comb_sgtf_within, aes(serial+0.1, n/sum(n), col="Omicron", shape="Omicron"), size=3) +
  geom_line(data=serial_density_lognormal_nsgtf_within, aes(x, y, col="Delta", lty="Delta"), lwd=1) +
  geom_line(data=serial_density_lognormal_sgtf_within, aes(x, y, col="Omicron", lty="Omicron"), lwd=1) +
  scale_x_continuous("Within-household\nserial intervals (days)", expand=c(0, 0), limits=c(-6, 16)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.23)) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_linetype_discrete("") +
  scale_shape_discrete("") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.75, 0.85),
    legend.background = element_rect(fill=NA)
  )

g2 <- ggplot(genden_nsgtf_within) +
  geom_line(aes(time, density), lwd=1) +
  geom_line(data=genden_sgtf_within, aes(time, density), lwd=1, lty=2, col="Orange") +
  scale_x_continuous("Within-household\ngeneration intervals (days)", expand=c(0, 0), limits=c(0, 14)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.38)) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_linetype_discrete("") +
  scale_shape_discrete("") +
  theme(
    panel.grid = element_blank()
  )

g3 <- ggplot(filter(fit_lognormal_r_comb_nsgtf_within, param=="mean")) +
  geom_line(aes(r, est), lwd=1) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_x_continuous("Delta growth rate (1/day)") +
  scale_y_continuous("Mean within-household\ngeneration interval (days)", limits=c(2.4, 4.1)) +
  theme(
    panel.grid = element_blank()
  )

g4 <- ggplot(filter(fit_lognormal_r_comb_sgtf_within, param=="mean")) +
  geom_line(aes(r, est), lwd=1, lty=2, col="orange") +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2, fill="orange") + 
  scale_x_continuous("Omicron growth rate (1/day)") +
  scale_y_continuous("Mean within-household\ngeneration interval (days)", limits=c(2.4, 4.1)) +
  theme(
    panel.grid = element_blank()
  )

g5 <- ggplot(serialdata_comb_nsgtf_between) +
  geom_point(aes(serial-0.1, n/sum(n), col="Delta", shape="Delta"), size=3) +
  geom_point(data=serialdata_comb_sgtf_between, aes(serial+0.1, n/sum(n), col="Omicron", shape="Omicron"), size=3) +
  geom_line(data=serial_density_lognormal_nsgtf_between, aes(x, y, col="Delta", lty="Delta"), lwd=1) +
  geom_line(data=serial_density_lognormal_sgtf_between, aes(x, y, col="Omicron", lty="Omicron"), lwd=1) +
  scale_x_continuous("Between-household\nserial intervals (days)", expand=c(0, 0), limits=c(-6, 16)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.23)) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_linetype_discrete("") +
  scale_shape_discrete("") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.75, 0.85),
    legend.background = element_rect(fill=NA)
  )

g6 <- ggplot(genden_nsgtf_between) +
  geom_line(aes(time, density), lwd=1) +
  geom_line(data=genden_sgtf_between, aes(time, density), lwd=1, lty=2, col="Orange") +
  scale_x_continuous("Between-household\ngeneration intervals (days)", expand=c(0, 0), limits=c(0, 14)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.38)) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_linetype_discrete("") +
  scale_shape_discrete("") +
  theme(
    panel.grid = element_blank()
  )

g7 <- ggplot(filter(fit_lognormal_r_comb_nsgtf_between, param=="mean")) +
  geom_line(aes(r, est), lwd=1) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_x_continuous("Delta growth rate (1/day)") +
  scale_y_continuous("Mean between-household\ngeneration interval (days)", limits=c(2.4, 4.1)) +
  theme(
    panel.grid = element_blank()
  )

g8 <- ggplot(filter(fit_lognormal_r_comb_sgtf_between, param=="mean")) +
  geom_line(aes(r, est), lwd=1, lty=2, col="orange") +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2, fill="orange") + 
  scale_x_continuous("Omicron growth rate (1/day)") +
  scale_y_continuous("Mean between-household\ngeneration interval (days)", limits=c(2.4, 4.1)) +
  theme(
    panel.grid = element_blank()
  )

gcomb1 <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, nrow=2, draw=FALSE, 
                    labels=c("A", "B", "C", "D", "E", "F", "G", "H"))

saveGG(gcomb1, width=12, heigh=6)
