library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=14))
library(egg)

library(shellpipes)
rpcall("figure_reproduction_advantage.Rout figure_reproduction_advantage.R advantage.rda")

sourceFiles()
loadEnvironments()

g2 <- ggplot(filter(advantagesumm, key %in% c("R_advantage_naive", "R_delta", "R_omicron_naive"))) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(time, ymin=lwr, ymax=upr, fill=key), alpha=0.2) +
  geom_line(aes(time, median, col=key), lwd=0.8) +
  annotate("text", x=Inf, y=Inf, label="Identical generation-interval distributions", hjust=1.01, vjust=1.3, family="Times") +
  scale_x_continuous("Date", breaks=2:10,
                     labels=c("Nov 28", "Dec 5", "Dec 12", "Dec 19", "Dec 26", "Jan 2", "Jan 9", "Jan 16",
                              "Jan 23")) +
  scale_y_log10("Reproduction number", breaks=c(0.5, 1, 2, 4), limits=c(0.5, 4), expand=c(0, 0)) +
  scale_color_manual(values=c("purple", "black", "orange"), labels=c("Advantage", "Delta", "Omicron")) +
  scale_fill_manual(values=c("purple", "black", "orange"), labels=c("Advantage", "Delta", "Omicron")) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

g1 <- ggplot(filter(advantagesumm, key %in% c("R_advantage", "R_delta", "R_omicron"))) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(time, ymin=lwr, ymax=upr, fill=key), alpha=0.2) +
  geom_line(aes(time, median, col=key), lwd=0.8) +
  annotate("text", x=Inf, y=Inf, label="Different generation-interval distributions", hjust=1.01, vjust=1.3, family="Times") +
  scale_x_continuous("Date", breaks=2:10,
                     labels=c("Nov 28", "Dec 5", "Dec 12", "Dec 19", "Dec 26", "Jan 2", "Jan 9", "Jan 16",
                              "Jan 23")) +
  scale_y_log10("Reproduction number", breaks=c(0.5, 1, 2, 4), limits=c(0.5, 4), expand=c(0, 0)) +
  scale_color_manual(values=c("purple", "black", "orange"), labels=c("Advantage", "Delta", "Omicron")) +
  scale_fill_manual(values=c("purple", "black", "orange"), labels=c("Advantage", "Delta", "Omicron")) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

g3 <- ggplot(advantagesumm2) +
  geom_path(aes(R_delta, R_advantage), lwd=1, arrow = arrow(length=unit(0.30,"cm"))) +
  scale_x_log10("Delta reproduction number") +
  scale_y_log10("Reproduction number ratio") +
  theme(
    panel.grid = element_blank()
  )

g4 <- ggplot(advantagesumm2) +
  geom_path(aes(R_omicron, R_advantage), lwd=1, arrow = arrow(length=unit(0.30,"cm")), col="black") +
  scale_x_log10("Omicron reproduction number") +
  scale_y_log10("Reproduction number ratio") +
  theme(
    panel.grid = element_blank()
  )

g5 <- ggplot(advantagesumm2) +
  geom_path(aes(R_delta, R_omicron), lwd=1, arrow = arrow(length=unit(0.30,"cm")), col="black") +
  scale_x_log10("Delta reproduction number") +
  scale_y_log10("Omicron reproduction number") +
  theme(
    panel.grid = element_blank()
  )

gfinal <- ggarrange(g1, g2, nrow=1, labels=c("A", "B"), draw=FALSE)

saveGG(gfinal, width=8.1, height=3)

gsupp <- ggarrange(g5, g3, g4, nrow=1, labels=c("A", "B", "C"), draw=FALSE)

ggsave("figure_reproduction_advantage_phase.pdf", gsupp, width=9, height=3)
