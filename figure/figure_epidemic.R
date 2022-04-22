library(vroom)
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=13))
library(mgcv)
library(mvtnorm)
library(egg)

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

g1 <- ggplot(cases_all) +
  geom_rect(xmin=as.Date("2021-12-13"), xmax=as.Date("2021-12-19"), ymin=-Inf, ymax=Inf, alpha=0.1, fill="gray") +
  geom_rect(xmin=as.Date("2021-12-20"), xmax=as.Date("2021-12-26"), ymin=-Inf, ymax=Inf, alpha=0.02, fill=4) +
  geom_point(aes(Date_of_publication, cases)) +
  geom_line(aes(Date_of_publication, rollmean), lty=1) +
  annotate("text", x=as.Date("2021-12-15")+1, y=75000, label=c("Week\n50"), family="Times", size=3) +
  annotate("text", x=as.Date("2021-12-22")+1, y=75000, label=c("Week\n51"), family="Times", size=3) +
  scale_x_date("Date", limits=c(as.Date("2021-11-28"), as.Date("2022-01-30")), 
               breaks=variant4$date,
               labels=c("Nov 28", "Dec 5", "Dec 12", "Dec 19", "Dec 26", "Jan 2", "Jan 9", "Jan 16",
                        "Jan 23", "Jan 30")) +
  scale_y_log10("Number of reported cases", limits=c(9000, NA),
                breaks=c(1e4, 2e4, 4e4, 8e4)) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

g2 <- ggplot(variant3) +
  geom_point(aes(date, omicron_prop, col="Omicron", shape="Omicron")) +
  geom_errorbar(aes(date, ymin=omicron_prop_lwr, ymax=omicron_prop_upr, col="Omicron"), width=0) +
  geom_point(aes(date, delta_prop, col="Delta", shape="Delta")) +
  geom_errorbar(aes(date, ymin=delta_prop_lwr, ymax=delta_prop_upr, col="Delta"), width=0) +
  scale_x_date("Date", breaks=variant4$date,
               labels=c("Nov 28", "Dec 5", "Dec 12", "Dec 19", "Dec 26", "Jan 2", "Jan 9", "Jan 16",
                        "Jan 23", "Jan 30")) +
  scale_y_continuous("Proportion detected") +
  scale_shape_discrete("") +
  scale_color_manual("", values=c("black", "orange")) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

gfit1 <- gam(log(delta_cases)~s(time, bs="cs"), data=variant4, method="REML")
gfit2 <- gam(log(omicron_cases)~s(time, bs="cs"), data=variant4, method="REML")

gfit1_p1 <- predict(gfit1, newdata = data.frame(time=seq(1, 10, by=1/7)), type = "lpmatrix")
gfit1_p2 <- predict(gfit1, newdata = data.frame(time=seq(1, 10, by=1/7)+0.01), type = "lpmatrix")
gfit2_p1 <- predict(gfit2, newdata = data.frame(time=seq(1, 10, by=1/7)), type = "lpmatrix")
gfit2_p2 <- predict(gfit2, newdata = data.frame(time=seq(1, 10, by=1/7)+0.01), type = "lpmatrix")

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
  median=apply(gfit1_post, 1, median),
  lwr=apply(gfit1_post, 1, quantile, 0.025),
  upr=apply(gfit1_post, 1, quantile, 0.975),
  type="Delta"
)

mean(delta_growth$median[datevec >= "2021-12-01" & datevec <= "2022-01-02"])

omicron_growth <- data.frame(
  time=seq(1, 10, by=1/7),
  median=apply(gfit2_post, 1, median),
  lwr=apply(gfit2_post, 1, quantile, 0.025),
  upr=apply(gfit2_post, 1, quantile, 0.975),
  type="Omicron"
)

mean(omicron_growth$median[datevec >= "2021-12-01" & datevec <= "2022-01-02"])

advantage_growth <- data.frame(
  time=seq(1, 10, by=1/7),
  median=apply(advantage_post, 1, median),
  lwr=apply(advantage_post, 1, quantile, 0.025),
  upr=apply(advantage_post, 1, quantile, 0.975),
  type="Advantage"
)

all_growth <- bind_rows(
  delta_growth, omicron_growth, advantage_growth
)

g3 <- ggplot(variant4) +
  geom_point(aes(date, omicron_cases, col="Omicron", shape="Omicron"))  +
  geom_smooth(aes(date, omicron_cases, col="Omicron", fill="Omicron"), alpha=0.2, method="gam", lwd=0.8)  +
  geom_point(aes(date, delta_cases, col="Delta", shape="Delta")) +
  geom_smooth(aes(date, delta_cases, col="Delta", fill="Delta"), alpha=0.2, method="gam", lwd=0.8)  +
  scale_shape_discrete("") +
  scale_fill_manual("", values=c("black", "orange")) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_x_date("Date", breaks=variant4$date,
               labels=c("Nov 28", "Dec 5", "Dec 12", "Dec 19", "Dec 26", "Jan 2", "Jan 9", "Jan 16",
                        "Jan 23", "Jan 30")) +
  scale_y_log10("Number of estimated cases") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

g4 <- ggplot(all_growth) +
  geom_hline(yintercept=0, lty=2) +
  geom_vline(xintercept=4, lty=2) +
  geom_vline(xintercept=8-1/7, lty=2) +
  annotate("text", x=4, y=0.29, hjust=-0.05, label="Lockdown\nintroduced", family="Times") +
  annotate("text", x=8-1/7, y=0.29, hjust=-0.05, label="Lockdown\nrelaxed", family="Times") +
  geom_ribbon(aes(time, ymin=lwr, ymax=upr, fill=type), alpha=0.2) +
  geom_line(aes(time, median, col=type), lwd=0.8) +
  scale_x_continuous("Date", breaks=1:10,
                     labels=c("Nov 28", "Dec 5", "Dec 12", "Dec 19", "Dec 26", "Jan 2", "Jan 9", "Jan 16",
                              "Jan 23", "Jan 30")) +
  scale_y_continuous("Growth rate (1/day)", limits=c(NA, 0.3)) +
  scale_color_manual(values=c("purple", "black", "orange")) +
  scale_fill_manual(values=c("purple", "black", "orange")) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

all_growth %>% filter(type=="Delta")

gtot <- ggarrange(g1, g2, g3, g4, nrow=2, labels=c("A", "B", "C", "D"), draw=FALSE)

ggsave("figure_epidemic.pdf", gtot, width=10, height=8)

