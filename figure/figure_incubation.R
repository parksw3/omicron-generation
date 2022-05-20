library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=14))
library(egg)
library(shellpipes)
rpcall("figure_incubation.Rout figure_incubation.R rdaout/calculate_incubation_r.rda")

sourceFiles()
loadEnvironments()

g1 <- ggplot(observed_nsgtf) +
  geom_line(aes(time, density, col="Delta", lty="Delta"), lwd=1) +
  geom_line(data=observed_sgtf, aes(time, density, col="Omicron", lty="Omicron"), lwd=1) +
  scale_x_continuous("Observed (backward) incubation period (days)", expand=c(0, 0)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.23)) +
  scale_color_manual("", values=c("black", "orange")) +
  scale_linetype_discrete("") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.title = element_blank()
  )

g2 <- ggplot(corrected_nsgtf) +
  geom_line(aes(time, density, col="Delta"), lwd=1) +
  geom_line(data=corrected_sgtf, aes(time, density, col="Omicron"), lwd=1, lty=2) +
  scale_x_continuous("Inferred (forward) incubation period (days)", expand=c(0, 0), limits=c(0, 14)) +
  scale_y_continuous("Density (1/day)", expand=c(0, 0), limits=c(0, 0.23)) +
  scale_color_manual(values=c("black", "orange")) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g3 <- ggplot(corrected_mean_nsgtf) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_line(aes(r, median), lwd=1) +
  scale_x_continuous("Delta growth rate (1/day)") +
  scale_y_continuous("Mean forward incubation period (days)", limits=c(3.3, 5.6)) +
  theme(
    panel.grid = element_blank()
  )

g4 <- ggplot(corrected_mean_sgtf) +
  geom_ribbon(aes(r, ymin=lwr, ymax=upr), alpha=0.2, fill="orange") + 
  geom_line(aes(r, median), lty=2, col="orange", lwd=1) +
  scale_x_continuous("Omicron growth rate (1/day)") +
  scale_y_continuous("Mean forward incubation period (days)", limits=c(3.3, 5.6)) +
  theme(
    panel.grid = element_blank()
  )

gfinal <- ggarrange(g1, g2, g3, g4, nrow=2, labels=c("A", "B", "C", "D"), draw=FALSE)

saveGG(gfinal, width=8, height=6)
