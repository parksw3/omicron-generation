library(vroom)
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(mgcv)
library(mvtnorm)
library(shellpipes)
## Files passed to shellpipes by make (or run this line interactively)
rpcall("cases.Rout cases.R")

sourceFiles()
loadEnvironments()

datevec <- seq(as.Date("2021-11-28"), as.Date("2022-01-30"), by=1)
weekbreak <- as.Date("2021-12-19") + 7 * (-4:6)

nsample <- 1000

cases <- vroom("data/COVID-19_aantallen_gemeente_per_dag.csv")
variant <- read_xlsx("data/variant-netherlands.xlsx")

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
gfit1_p2 <- predict(gfit1, newdata = data.frame(time=seq(1, 10, by=1/14)+0.01), type = "lpmatrix")
gfit2_p1 <- predict(gfit2, newdata = data.frame(time=seq(1, 10, by=1/14)), type = "lpmatrix")
gfit2_p2 <- predict(gfit2, newdata = data.frame(time=seq(1, 10, by=1/14)+0.01), type = "lpmatrix")

gfit1_Xp <- (gfit1_p2 - gfit1_p1) / 0.01/7
gfit2_Xp <- (gfit2_p2 - gfit2_p1) / 0.01/7

set.seed(101)
gfit1_sim <- rmvnorm(nsample, mean = coef(gfit1), sigma = vcov(gfit1))
gfit2_sim <- rmvnorm(nsample, mean = coef(gfit2), sigma = vcov(gfit2))

gfit1_post <- apply(gfit1_sim, 1, function(x) {gfit1_Xp %*% x})
gfit2_post <- apply(gfit2_sim, 1, function(x) {gfit2_Xp %*% x})
advantage_post <- gfit2_post - gfit1_post

delta_growth <- data.frame(
  time=seq(1, 10, by=1/7),
  median=apply(gfit1_post, 1, median)[seq(1, nrow(gfit1_post), by=2)],
  lwr=apply(gfit1_post, 1, quantile, 0.025)[seq(1, nrow(gfit1_post), by=2)],
  upr=apply(gfit1_post, 1, quantile, 0.975)[seq(1, nrow(gfit1_post), by=2)],
  type="Delta"
)

mean(delta_growth$median[datevec >= "2021-12-01" & datevec <= "2022-01-02"])

omicron_growth <- data.frame(
  time=seq(1, 10, by=1/7),
  median=apply(gfit2_post, 1, median)[seq(1, nrow(gfit1_post), by=2)],
  lwr=apply(gfit2_post, 1, quantile, 0.025)[seq(1, nrow(gfit1_post), by=2)],
  upr=apply(gfit2_post, 1, quantile, 0.975)[seq(1, nrow(gfit1_post), by=2)],
  type="Omicron"
)

mean(omicron_growth$median[datevec >= "2021-12-01" & datevec <= "2022-01-02"])

advantage_growth <- data.frame(
  time=seq(1, 10, by=1/7),
  median=apply(advantage_post, 1, median)[seq(1, nrow(gfit1_post), by=2)],
  lwr=apply(advantage_post, 1, quantile, 0.025)[seq(1, nrow(gfit1_post), by=2)],
  upr=apply(advantage_post, 1, quantile, 0.975)[seq(1, nrow(gfit1_post), by=2)],
  type="Advantage"
)

all_growth <- bind_rows(
  delta_growth, omicron_growth, advantage_growth
)

# all_growth %>% filter(type=="Delta")

saveEnvironment()
