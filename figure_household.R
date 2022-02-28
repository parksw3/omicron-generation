library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
load("fit_lognormal_base.rda")
load("simulate_generation_household.rda")

logmean_nsgtf <- coef(fit_lognormal_base_50_nsgtf_within)[1]
logsd_nsgtf <- coef(fit_lognormal_base_50_nsgtf_within)[2]
mean_nsgtf <- exp(logmean_nsgtf + logsd_nsgtf^2/2)

logmean_sgtf <- coef(fit_lognormal_base_50_sgtf_within)[1]
logsd_sgtf <- coef(fit_lognormal_base_50_sgtf_within)[2]
mean_sgtf <- exp(logmean_sgtf + logsd_sgtf^2/2)

simulate_generation_household2 <- simulate_generation_household %>%
  mutate(
    k=paste0("k=", k),
    ratio=paste0("Reproduction number ratio: ", ratio),
    type=factor(type, labels=c("Egocentric", "Local"))
  )

g1 <- ggplot(simulate_generation_household2) +
  geom_hline(yintercept=mean_nsgtf, lty = "dotted") +
  geom_hline(yintercept=mean_sgtf, lty = "dotted", col="orange") +
  geom_ribbon(aes(household, ymin=lwr, ymax=upr, fill=strain, col=strain, lty=type), alpha=0.1) +
  geom_line(aes(household, mean, col=strain, lty=type)) +
  scale_x_continuous("Network sizes", breaks=1:5*4) +
  scale_y_continuous("Mean generation interval (days)") +
  scale_color_manual("Variant", values=c("black", "orange")) +
  scale_fill_manual("Variant", values=c("black", "orange")) +
  scale_linetype_discrete("Simulation") +
  facet_grid(ratio~k) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank()
  )

ggsave("figure_household.pdf", g1, width=8, height=6)
