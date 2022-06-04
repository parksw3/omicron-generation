library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=13))
library(egg)
library(shellpipes)

sourceFiles()
loadEnvironments()

g1 <- ggplot(cases_all) +
  annotate("rect", xmin=as.Date("2021-12-13"), xmax=as.Date("2021-12-19"), ymin=0, ymax=Inf, alpha=0.5, fill="gray") +
  annotate("rect", xmin=as.Date("2021-12-20"), xmax=as.Date("2021-12-26"), ymin=0, ymax=Inf, alpha=0.5, fill=4) +
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
  annotate("rect", xmin=as.Date("2021-12-13"), xmax=as.Date("2021-12-19"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="gray") +
  annotate("rect", xmin=as.Date("2021-12-20"), xmax=as.Date("2021-12-26"), ymin=-Inf, ymax=Inf, alpha=0.5, fill=4) +
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

g3 <- ggplot(variant4) +
  annotate("rect", xmin=as.Date("2021-12-13"), xmax=as.Date("2021-12-19"), ymin=0, ymax=Inf, alpha=0.5, fill="gray") +
  annotate("rect", xmin=as.Date("2021-12-20"), xmax=as.Date("2021-12-26"), ymin=0, ymax=Inf, alpha=0.5, fill=4) +
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
  annotate("rect", xmin=3+1/7, xmax=4, ymin=-Inf, ymax=Inf, alpha=0.5, fill="gray") +
  annotate("rect", xmin=4+1/7, xmax=5, ymin=-Inf, ymax=Inf, alpha=0.5, fill=4) +
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

gtot <- ggarrange(g1, g2, g3, g4, nrow=2, labels=c("A", "B", "C", "D"), draw=FALSE)

saveGG(gtot, width=10, height=8)
