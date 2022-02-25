library(vroom)
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(mgcv)
library(mvtnorm)
library(egg)
load("fit_lognormal_base.rda")

weekbreak <- as.Date("2021-12-19") + 7 * (-4:6)

nsample <- 1000

cases <- vroom("COVID-19_aantallen_gemeente_per_dag.csv")
variant <- read_xlsx("variant-netherlands.xlsx")

cases_all <- cases %>%
  group_by(Date_of_publication) %>%
  summarize(
    cases=sum(Total_reported)
  ) %>%
  arrange(Date_of_publication) %>%
  mutate(
    rollmean=rollmean(cases, 7, fill=NA)
  ) %>%
  filter(Date_of_publication >= "2021-11-22", Date_of_publication <= "2022-01-30")

cases_all_weekly <- cases_all %>%
  mutate(
    group=cut(Date_of_publication, weekbreak+1)
  ) %>%
  group_by(group) %>%
  summarize(
    cases=sum(cases),
    date=max(as.Date(Date_of_publication))
  )

variant2 <- data.frame(
  omicron=unlist(variant[9,-c(1:2, 13)]),
  delta=unlist(variant[5,-c(1:2, 13)]),
  total=unlist(variant[1,-c(1:2, 13)]),
  week=c(4, 3, 2, 1, 52, 51, 50, 49, 48, 47),
  year=c(2022, 2022, 2022, 2022, 2021, 2021, 2021, 2021, 2021, 2021),
  date=rev(cases_all_weekly$date)
)

variant3 <- variant2 %>%
  group_by(year, week) %>%
  mutate(
    omicron_prop=omicron/total,
    omicron_prop_lwr=binom.test(omicron, total)[[4]][1],
    omicron_prop_upr=binom.test(omicron, total)[[4]][2],
    delta_prop=delta/total,
    delta_prop_lwr=binom.test(delta, total)[[4]][1],
    delta_prop_upr=binom.test(delta, total)[[4]][2]
  ) %>%
  merge(
    cases_all_weekly
  ) %>%
  arrange(date) %>%
  mutate(
    time=1:n()
  )

variant4 <- variant3 %>%
  mutate(
    omicron_cases=omicron_prop * cases,
    delta_cases=delta_prop * cases
  )

gfit1 <- gam(log(delta_cases)~s(time, bs="cs"), data=variant4, method="REML")
gfit2 <- gam(log(omicron_cases)~s(time, bs="cs"), data=variant4, method="REML")

gfit1_p1 <- predict(gfit1, newdata = data.frame(time=seq(1, 10, by=1/14)), type = "lpmatrix")
gfit2_p1 <- predict(gfit2, newdata = data.frame(time=seq(1, 10, by=1/14)), type = "lpmatrix")

set.seed(101)
gfit1_sim <- rmvnorm(nsample, mean = coef(gfit1), sigma = vcov(gfit1))
gfit2_sim <- rmvnorm(nsample, mean = coef(gfit2), sigma = vcov(gfit2))
lognormal_sim1 <- rmvnorm(nsample, mean=coef(fit_lognormal_base_50_nsgtf_within)[1:2], sigma=vcov(fit_lognormal_base_50_nsgtf_within))
lognormal_sim2 <- rmvnorm(nsample, mean=coef(fit_lognormal_base_50_sgtf_within)[1:2], sigma=vcov(fit_lognormal_base_50_sgtf_within))

advantagedata <- lapply(1:nsample, function(x) {
  i1 <- c(exp(gfit1_p1 %*% c(gfit1_sim[x,]))/14)
  i2 <- c(exp(gfit2_p1 %*% c(gfit2_sim[x,]))/14)
  
  gen1 <- diff(plnorm(seq(0, 14, by=0.5), lognormal_sim1[x,1], lognormal_sim1[x,2]))
  gen2 <- diff(plnorm(seq(0, 14, by=0.5), lognormal_sim2[x,1], lognormal_sim2[x,2]))
  
  n <- length(gen1)
  
  R_delta <- tail(i1, -n)/sapply(1:(length(i1)-n), function(y) sum(i1[y:(y+n-1)] * rev(gen1)))
  R_omicron <- tail(i2, -n)/sapply(1:(length(i2)-n), function(y) sum(i2[y:(y+n-1)] * rev(gen2)))
  
  
  data.frame(
    R_delta=R_delta,
    R_omicron=R_omicron,
    R_advantage=R_omicron/R_delta,
    sim=x,
    time=tail(seq(1, 10, by=1/14), -n)
  )
}) %>%
  bind_rows

advantagesumm <- advantagedata %>%
  gather(key, value, -time, -sim) %>%
  group_by(time, key) %>%
  summarize(
    median=median(value),
    lwr=quantile(value, 0.025),
    upr=quantile(value, 0.975)
  ) %>%
  mutate(
    key=factor(key, labels=c("Advantage", "Delta", "Omicron"))
  )

g1 <- ggplot(advantagesumm) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(time, ymin=lwr, ymax=upr, fill=key), alpha=0.2) +
  geom_line(aes(time, median, col=key), lwd=0.8) +
  scale_x_continuous("Date", breaks=2:10,
                     labels=c("Nov 28", "Dec 5", "Dec 12", "Dec 19", "Dec 26", "Jan 2", "Jan 9", "Jan 16",
                              "Jan 23")) +
  scale_y_continuous("Reproduction number") +
  scale_color_manual(values=c("purple", "black", "orange")) +
  scale_fill_manual(values=c("purple", "black", "orange")) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )

ggsave("figure_reproduction_advantage.pdf", g1, width=6, height=4)
