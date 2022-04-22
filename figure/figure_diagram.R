library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)
library(shellpipes)
startGraphics()

size <- 4
stroke <- 1
epsilon <- 0.08

g1 <- ggplot() +
  annotate("text", x=5, y=0.55, label="Same symptom onset time", family="Times") +
  geom_rect(aes(xmin=4.3, xmax=5.7, ymin=-3.7, ymax=0.4), col="black", lty=3, fill=NA, lwd=stroke) +
  geom_segment(aes(x=-3, xend=10, y=0, yend=0), col="black", lwd=stroke) +
  geom_segment(aes(x=-3, xend=10, y=-1.5, yend=-1.5), col="black", lwd=stroke) +
  geom_segment(aes(x=-3, xend=10, y=-3, yend=-3), col="black", lwd=stroke) +
  geom_segment(aes(x=-3, xend=10, y=-4.5, yend=-4.5), col="black", lwd=stroke) +
  geom_segment(aes(x=3, xend=3, y=0, yend=-1.5), col="black", lty=2) +
  geom_segment(aes(x=2.5, xend=2.5, y=-3, yend=-4.5), col="black", lty=2) +
  geom_rect(aes(xmin=-4, xmax=-2, ymin=-0.2, ymax=0.2), fill="gray90", col="black", size=stroke) +
  annotate("text", x=-3, y=0, label="Infector A", family="Times") +
  geom_rect(aes(xmin=-4, xmax=-2, ymin=-1.7, ymax=-1.3), fill="gray90", col="black", size=stroke) +
  annotate("text", x=-3, y=-1.5, label="Infectee B", family="Times") +
  geom_rect(aes(xmin=-4, xmax=-2, ymin=-0.2-3, ymax=0.2-3), fill="gray90", col="black", size=stroke) +
  annotate("text", x=-3, y=0-3, label="Infector C", family="Times") +
  geom_rect(aes(xmin=-4, xmax=-2, ymin=-1.7-3, ymax=-1.3-3), fill="gray90", col="black", size=stroke) +
  annotate("text", x=-3, y=-1.5-3, label="Infectee D", family="Times") +
  annotate("text", x=0, y=0.25, label="Infection", family="Times") +
  annotate("text", x=3, y=0.25, label="Transmission, A to B", family="Times") +
  annotate("text", x=5, y=0.25, label="Symptom", family="Times") +
  geom_rect(aes(xmin=0+epsilon, xmax=5-epsilon, ymin=-0.5+epsilon, ymax=-0.1-epsilon), fill="#FFDC89", col="black", size=stroke, alpha=1) +
  annotate("text", x=2.5, y=-0.3, label="Incubation period", family="Times") +
  geom_rect(aes(xmin=5+epsilon, xmax=8-epsilon, ymin=-0.5+epsilon, ymax=-0.1-epsilon), fill="#A5BDFF", col="black", size=stroke, alpha=1) +
  annotate("text", x=6.5, y=-0.3, label="Serial interval", family="Times") +
  geom_rect(aes(xmin=0+epsilon, xmax=3-epsilon, ymin=-0.9+epsilon, ymax=-0.5-epsilon), fill="#CB93E8", col="black", size=stroke, alpha=1) +
  annotate("text", x=1.5, y=-0.7, label="Generation interval", family="Times") +
  geom_point(aes(0, 0), size=size, shape=21, fill="gray", stroke=stroke) +
  geom_point(aes(3, 0), size=size, shape=22, fill="#CB93E8", stroke=stroke) +
  geom_point(aes(5, 0), size=size, shape=23, fill="#FFDC89", stroke=stroke) +
  geom_rect(aes(xmin=2+epsilon, xmax=4-epsilon, ymin=-1.25-0.1, ymax=-1.25+0.1), fill="white", col="white") +
  annotate("text", x=3, y=-1.25, label="Infection", family="Times") +
  annotate("text", x=8, y=-1.25, label="Symptom", family="Times") +
  geom_point(aes(3, -1.5), size=size, shape=21, fill="gray", stroke=stroke) +
  geom_point(aes(8, -1.5), size=size, shape=23, fill="#FFDC89", stroke=stroke) +
  geom_rect(aes(xmin=3+epsilon, xmax=8-epsilon, ymin=-2+epsilon, ymax=-1.6-epsilon), fill="#FFDC89", col="black", size=stroke, alpha=1) +
  annotate("text", x=5.5, y=-1.8, label="Incubation period", family="Times") +
  annotate("text", x=-1, y=0.25-3, label="Infection", family="Times") +
  annotate("text", x=2.5, y=0.25-3, label="Transmission, C to D", family="Times") +
  annotate("text", x=5, y=0.25-3, label="Symptom", family="Times") +
  geom_rect(aes(xmin=-1+epsilon, xmax=5-epsilon, ymin=-0.5+epsilon-3, ymax=-0.1-epsilon-3), fill="#FFDC89", col="black", size=stroke, alpha=1) +
  annotate("text", x=2, y=-0.3-3, label="Incubation period", family="Times") +
  geom_rect(aes(xmin=5+epsilon, xmax=9-epsilon, ymin=-0.5+epsilon-3, ymax=-0.1-epsilon-3), fill="#A5BDFF", col="black", size=stroke, alpha=1) +
  annotate("text", x=7, y=-0.3-3, label="Serial interval", family="Times") +
  geom_rect(aes(xmin=-1+epsilon, xmax=2.5-epsilon, ymin=-0.9+epsilon-3, ymax=-0.5-epsilon-3), fill="#CB93E8", col="black", size=stroke, alpha=1) +
  annotate("text", x=0.75, y=-0.7-3, label="Generation interval", family="Times") +
  geom_point(aes(-1, 0-3), size=size, shape=21, fill="gray", stroke=stroke) +
  geom_point(aes(2.5, 0-3), size=size, shape=22, fill="#CB93E8", stroke=stroke) +
  geom_point(aes(5, 0-3), size=size, shape=23, fill="#FFDC89", stroke=stroke) +
  geom_rect(aes(xmin=1.5+epsilon, xmax=3.5-epsilon, ymin=-4.25-0.1, ymax=-4.25+0.1), fill="white", col="white") +
  annotate("text", x=2.5, y=-1.25-3, label="Infection", family="Times") +
  annotate("text", x=9, y=-1.25-3, label="Symptom", family="Times") +
  geom_point(aes(2.5, -1.5-3), size=size, shape=21, fill="gray", stroke=stroke) +
  geom_point(aes(9, -1.5-3), size=size, shape=23, fill="#FFDC89", stroke=stroke) +
  geom_rect(aes(xmin=2.5+epsilon, xmax=9-epsilon, ymin=-2+epsilon-3, ymax=-1.6-epsilon-3), fill="#FFDC89", col="black", size=stroke, alpha=1) +
  annotate("text", x=5.75, y=-1.8-3, label="Incubation period", family="Times") +
  annotate("text", x=-4, y=0.8, label="Symptom-based infector cohort", size=6, hjust=0, family="Times") +
  scale_x_continuous(limits=c(-4, 10)) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )
  
g2 <- ggplot() +
  annotate("text", x=0, y=0.55, label="Same infection time", family="Times") +
  geom_rect(aes(xmin=-0.7, xmax=0.7, ymin=-3.7, ymax=0.4), col="black", lty=3, fill=NA, lwd=stroke) +
  geom_segment(aes(x=-3, xend=10, y=0, yend=0), col="black", lwd=stroke) +
  geom_segment(aes(x=-3, xend=10, y=-1.5, yend=-1.5), col="black", lwd=stroke) +
  geom_segment(aes(x=-3, xend=10, y=-3, yend=-3), col="black", lwd=stroke) +
  geom_segment(aes(x=-3, xend=10, y=-4.5, yend=-4.5), col="black", lwd=stroke) +
  geom_segment(aes(x=3, xend=3, y=0, yend=-1.5), col="black", lty=2) +
  geom_segment(aes(x=2.5+1, xend=2.5+1, y=-3, yend=-4.5), col="black", lty=2) +
  geom_rect(aes(xmin=-4, xmax=-2, ymin=-0.2, ymax=0.2), fill="gray90", col="black", size=stroke) +
  annotate("text", x=-3, y=0, label="Infector E", family="Times") +
  geom_rect(aes(xmin=-4, xmax=-2, ymin=-1.7, ymax=-1.3), fill="gray90", col="black", size=stroke) +
  annotate("text", x=-3, y=-1.5, label="Infectee F", family="Times") +
  geom_rect(aes(xmin=-4, xmax=-2, ymin=-0.2-3, ymax=0.2-3), fill="gray90", col="black", size=stroke) +
  annotate("text", x=-3, y=0-3, label="Infector G", family="Times") +
  geom_rect(aes(xmin=-4, xmax=-2, ymin=-1.7-3, ymax=-1.3-3), fill="gray90", col="black", size=stroke) +
  annotate("text", x=-3, y=-1.5-3, label="Infectee H", family="Times") +
  annotate("text", x=0, y=0.25, label="Infection", family="Times") +
  annotate("text", x=3, y=0.25, label="Transmission, E to F", family="Times") +
  annotate("text", x=5, y=0.25, label="Symptom", family="Times") +
  geom_rect(aes(xmin=0+epsilon, xmax=5-epsilon, ymin=-0.5+epsilon, ymax=-0.1-epsilon), fill="#FFDC89", col="black", size=stroke, alpha=1) +
  annotate("text", x=2.5, y=-0.3, label="Incubation period", family="Times") +
  geom_rect(aes(xmin=5+epsilon, xmax=8-epsilon, ymin=-0.5+epsilon, ymax=-0.1-epsilon), fill="#A5BDFF", col="black", size=stroke, alpha=1) +
  annotate("text", x=6.5, y=-0.3, label="Serial interval", family="Times") +
  geom_rect(aes(xmin=0+epsilon, xmax=3-epsilon, ymin=-0.9+epsilon, ymax=-0.5-epsilon), fill="#CB93E8", col="black", size=stroke, alpha=1) +
  annotate("text", x=1.5, y=-0.7, label="Generation interval", family="Times") +
  geom_point(aes(0, 0), size=size, shape=21, fill="gray", stroke=stroke) +
  geom_point(aes(3, 0), size=size, shape=22, fill="#CB93E8", stroke=stroke) +
  geom_point(aes(5, 0), size=size, shape=23, fill="#FFDC89", stroke=stroke) +
  geom_rect(aes(xmin=2+epsilon, xmax=4-epsilon, ymin=-1.25-0.1, ymax=-1.25+0.1), fill="white", col="white") +
  annotate("text", x=3, y=-1.25, label="Infection", family="Times") +
  annotate("text", x=8, y=-1.25, label="Symptom", family="Times") +
  geom_point(aes(3, -1.5), size=size, shape=21, fill="gray", stroke=stroke) +
  geom_point(aes(8, -1.5), size=size, shape=23, fill="#FFDC89", stroke=stroke) +
  geom_rect(aes(xmin=3+epsilon, xmax=8-epsilon, ymin=-2+epsilon, ymax=-1.6-epsilon), fill="#FFDC89", col="black", size=stroke, alpha=1) +
  annotate("text", x=5.5, y=-1.8, label="Incubation period", family="Times") +
  annotate("text", x=-1+1, y=0.25-3, label="Infection", family="Times") +
  annotate("text", x=2.5+1, y=0.25-3, label="Transmission, G to H", family="Times") +
  annotate("text", x=5+1, y=0.25-3, label="Symptom", family="Times") +
  geom_rect(aes(xmin=-1+epsilon+1, xmax=5-epsilon+1, ymin=-0.5+epsilon-3, ymax=-0.1-epsilon-3), fill="#FFDC89", col="black", size=stroke, alpha=1) +
  annotate("text", x=2+1, y=-0.3-3, label="Incubation period", family="Times") +
  geom_rect(aes(xmin=5+epsilon+1, xmax=9-epsilon+1, ymin=-0.5+epsilon-3, ymax=-0.1-epsilon-3), fill="#A5BDFF", col="black", size=stroke, alpha=1) +
  annotate("text", x=7+1, y=-0.3-3, label="Serial interval", family="Times") +
  geom_rect(aes(xmin=-1+epsilon+1, xmax=2.5-epsilon+1, ymin=-0.9+epsilon-3, ymax=-0.5-epsilon-3), fill="#CB93E8", col="black", size=stroke, alpha=1) +
  annotate("text", x=0.75+1, y=-0.7-3, label="Generation interval", family="Times") +
  geom_point(aes(-1+1, 0-3), size=size, shape=21, fill="gray", stroke=stroke) +
  geom_point(aes(2.5+1, 0-3), size=size, shape=22, fill="#CB93E8", stroke=stroke) +
  geom_point(aes(5+1, 0-3), size=size, shape=23, fill="#FFDC89", stroke=stroke) +
  geom_rect(aes(xmin=1.5+1+epsilon, xmax=3.5+1-epsilon, ymin=-4.25-0.1, ymax=-4.25+0.1), fill="white", col="white") +
  annotate("text", x=2.5+1, y=-1.25-3, label="Infection", family="Times") +
  annotate("text", x=9+1, y=-1.25-3, label="Symptom", family="Times") +
  geom_point(aes(2.5+1, -1.5-3), size=size, shape=21, fill="gray", stroke=stroke) +
  geom_point(aes(9+1, -1.5-3), size=size, shape=23, fill="#FFDC89", stroke=stroke) +
  geom_rect(aes(xmin=2.5+1+epsilon, xmax=9+1-epsilon, ymin=-2+epsilon-3, ymax=-1.6-epsilon-3), fill="#FFDC89", col="black", size=stroke, alpha=1) +
  annotate("text", x=5.75+1, y=-1.8-3, label="Incubation period", family="Times") +
  annotate("text", x=-4, y=0.8, label="Infection-based infector cohort", size=6, hjust=0, family="Times") +
  scale_x_continuous(limits=c(-4, 10)) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

gcomb <- ggarrange(g1, g2, nrow=2, draw=FALSE, labels=c("A", "B"))
saveGG(gcomb)
