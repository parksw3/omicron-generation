library(dplyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(gridExtra)
source("serialfun.R")
load("calculate_incubation_mle.rda")
load("fit_lognormal_base.rda")
load("fit_lognormal_r.rda")
load("fit_lognormal_r_50_between.rda")
load("fit_lognormal_r_51_between.rda")
load("fit_lognormal_r_51.rda")

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
  y=do.call(serialfun_lognormal, c(list(x=xvec), as.list(coef(fit_lognormal_base_50_nsgtf_within)))),
  type="Delta"
)

serial_density_lognormal_sgtf <- data.frame(
  x=xvec,
  y=do.call(serialfun_lognormal, c(list(x=xvec), as.list(coef(fit_lognormal_base_50_sgtf_within)))),
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
    do.call(kernelfun_lognormal, c(list(x=x, y=ivec), as.list(coef(fit_lognormal_base_50_nsgtf_within)[-6])))
  }))
) %>%
  mutate(
    z=ifelse(is.finite(z), z, 0)
  )

kernel_lognormal_sgtf <- data.frame(
  gen=rep(gvec, each=length(ivec)),
  inc=rep(ivec, length(gvec)),
  z=c(sapply(gvec, function(x) {
    do.call(kernelfun_lognormal, c(list(x=x, y=ivec), as.list(coef(fit_lognormal_base_50_sgtf_within)[-6])))
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
  density=dlnorm(time, filter(fit_lognormal_r_50_nsgtf_within, r==-0.05, param=="logmean")$est, filter(fit_lognormal_r_50_nsgtf_within, r==-0.05, param=="logsd")$est)
)

genden_sgtf <- tibble(
  time=seq(0, 14, by=0.1),
  density=dlnorm(time, filter(fit_lognormal_r_50_sgtf_within, round(r, 2)==0.15, param=="logmean")$est, filter(fit_lognormal_r_50_sgtf_within, round(r, 2)==0.15, param=="logsd")$est)
)

1/sum(genden_sgtf$density * exp(-0.15 * genden_sgtf$time) * 0.1)
1/sum(serial_density_lognormal_sgtf$y * exp(-0.15 * serial_density_lognormal_sgtf$x) * 0.2)

1/sum(genden_nsgtf$density * exp(0.05 * genden_nsgtf$time) * 0.1)
1/sum(serial_density_lognormal_nsgtf$y * exp(0.05 * serial_density_lognormal_nsgtf$x) * 0.2)

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

g3 <- ggplot(filter(fit_lognormal_r_50_nsgtf_within, param=="mean")) +
  geom_line(aes(r, est), lwd=1) +
  geom_line(aes(r, lwr), lwd=1) +
  geom_line(aes(r, upr), lwd=1) +
  geom_line(data=filter(fit_lognormal_r_50_sgtf_within, param=="mean"), aes(r-0.2, est), lty=2, col="orange", lwd=1) +
  geom_line(data=filter(fit_lognormal_r_50_sgtf_within, param=="mean"), aes(r-0.2, lwr), lty=2, col="orange", lwd=1) +
  geom_line(data=filter(fit_lognormal_r_50_sgtf_within, param=="mean"), aes(r-0.2, upr), lty=2, col="orange", lwd=1) +
  scale_x_continuous("Delta growth rate (1/day)",
                     sec.axis = sec_axis(trans=~.+0.2, "Omicron growth rate (1/day)"), expand=c(0, 0)) +
  scale_y_continuous("Mean generation interval (days)") +
  theme(
    panel.grid = element_blank(),
    axis.line.x.top = element_line(color="orange"),
    axis.ticks.x.top = element_line(color="orange"),
    axis.text.x.top = element_text(color="orange"),
    axis.title.x.top = element_text(color="orange")
  )

allfits <- bind_rows(
  fit_lognormal_r_50_nsgtf_between,
  fit_lognormal_r_51_nsgtf_between,
  fit_lognormal_r_51_nsgtf_within,
  mutate(fit_lognormal_r_50_nsgtf_within, week=50, household="within"),
  fit_lognormal_r_50_sgtf_between,
  fit_lognormal_r_51_sgtf_between,
  fit_lognormal_r_51_sgtf_within,
  mutate(fit_lognormal_r_50_sgtf_within, week=50, household="within")
) %>%
  mutate(
    week=factor(week, levels=c(50, 51),
                labels=c("Week 50", "Week 51")),
    household=factor(household, levels=c("within", "between"),
                     labels=c("Within-household pairs", "Between-household pairs"))
  )

g4 <- ggplot(filter(allfits, param=="mean", type=="Delta")) +
  geom_line(aes(r, est), lwd=1, col="black") +
  geom_line(aes(r, lwr), lwd=1, col="black") +
  geom_line(aes(r, upr), lwd=1, col="black") +
  geom_line(data=filter(allfits, param=="mean", type=="Omicron"), aes(r-0.2, est), lty=2, col="orange", lwd=1) +
  geom_line(data=filter(allfits, param=="mean", type=="Omicron"), aes(r-0.2, lwr), lty=2, col="orange", lwd=1) +
  geom_line(data=filter(allfits, param=="mean", type=="Omicron"), aes(r-0.2, upr), lty=2, col="orange", lwd=1) +
  facet_grid(household~week) +
  scale_x_continuous("Delta growth rate (1/day)",
                     sec.axis = sec_axis(trans=~.+0.2, "Omicron growth rate (1/day)"), expand=c(0, 0),
                     breaks=c(-0.1, -0.08, -0.06, -0.04, -0.02, 0)) +
  scale_y_continuous("Mean generation interval (days)") +
  theme(
    panel.grid = element_blank(),
    axis.line.x.top = element_line(color="orange"),
    axis.ticks.x.top = element_line(color="orange"),
    axis.text.x.top = element_text(color="orange"),
    axis.title.x.top = element_text(color="orange"),
    panel.spacing = unit(0.8, "cm")
  )

gcomb1 <- ggarrange(g1, g2, nrow=2, draw=FALSE, 
                    labels=c("A", "B"))

gcomb2 <- ggarrange(g4, nrow=1, draw=FALSE, 
                    labels=c("C"))

gfinal <- arrangeGrob(gcomb1, gcomb2, widths=c(1, 2))

ggsave("figure_compare.pdf", gfinal, width=12, heigh=8)
