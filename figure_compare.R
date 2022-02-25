library(dplyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("serialfun.R")
load("calculate_incubation_mle.rda")
load("fit_lognormal_base.rda")
load("fit_lognormal_r.rda")

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
  scale_y_continuous("Density", expand=c(0, 0), limits=c(0, 0.22)) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_linetype_discrete("") +
  scale_shape_discrete("") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill=NA)
  )

g2 <- ggplot(filter(fit_lognormal_r_50_nsgtf_within, param=="mean")) +
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

g3 <- ggplot(kernel_lognormal_nsgtf) +
  geom_tile(aes(inc, gen,fill=z)) +
  scale_x_continuous("Forward incubation period (days)", expand=c(0, 0)) +
  scale_y_continuous("Forward generation interval (days)", expand=c(0, 0)) +
  annotate("text", 0, 16, label="Delta", color="white", hjust=0, vjust=1, size=6, family="Times") +
  scale_fill_viridis_c("Density", limits=c(0, 0.15)) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.7),
    legend.background = element_rect(fill=NA, color="white"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white"),
    legend.box = "white"
  )

g4 <- ggplot(kernel_lognormal_sgtf) +
  geom_tile(aes(inc, gen,fill=z)) +
  scale_x_continuous("Forward incubation period (days)", expand=c(0, 0)) +
  scale_y_continuous("Forward generation interval (days)", expand=c(0, 0)) +
  annotate("text", 0, 16, label="Omicron", color="white", hjust=0, vjust=1, size=6, family="Times") +
  scale_fill_viridis_c("Density", limits=c(0, 0.15)) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.7),
    legend.background = element_rect(fill=NA, color="white"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white"),
    legend.box = "white"
  )

gfinal <- ggarrange(g1, g2, g3, g4, nrow=2, draw=FALSE, 
                    labels=c("A", "B", "C", "D"))

ggsave("figure_compare.pdf", gfinal, width=10, heigh=8)
