library(dplyr)
library(readxl)

serialdata <- read_xlsx("data/serial-netherlands.xlsx")

serialdata_50_sgtf_within <- serialdata %>%
  filter(week==50, strain=="SGTF", household=="within")

serialdata_50_nsgtf_within <- serialdata %>%
  filter(week==50, strain=="non-SGTF", household=="within")

serialdata_50_sgtf_between <- serialdata %>%
  filter(week==50, strain=="SGTF", household=="between")

serialdata_50_nsgtf_between <- serialdata %>%
  filter(week==50, strain=="non-SGTF", household=="between")

serialdata_51_sgtf_within <- serialdata %>%
  filter(week==51, strain=="SGTF", household=="within")

serialdata_51_nsgtf_within <- serialdata %>%
  filter(week==51, strain=="non-SGTF", household=="within")

serialdata_51_sgtf_between <- serialdata %>%
  filter(week==51, strain=="SGTF", household=="between")

serialdata_51_nsgtf_between <- serialdata %>%
  filter(week==51, strain=="non-SGTF", household=="between")

serialdata_comb_sgtf_within <- serialdata %>%
  filter(strain=="SGTF", household=="within") %>%
  group_by(serial, strain, household) %>%
  summarize(
    n=sum(n)
  )

serialdata_comb_nsgtf_within <- serialdata %>%
  filter(strain=="non-SGTF", household=="within") %>%
  group_by(serial, strain, household) %>%
  summarize(
    n=sum(n)
  )

serialdata_comb_sgtf_between <- serialdata %>%
  filter(strain=="SGTF", household=="between") %>%
  group_by(serial, strain, household) %>%
  summarize(
    n=sum(n)
  )

serialdata_comb_nsgtf_between <- serialdata %>%
  filter(strain=="non-SGTF", household=="between") %>%
  group_by(serial, strain, household) %>%
  summarize(
    n=sum(n)
  )

data_50_sgtf_within <- rep(serialdata_50_sgtf_within$serial, serialdata_50_sgtf_within$n)
data_50_nsgtf_within <- rep(serialdata_50_nsgtf_within$serial, serialdata_50_nsgtf_within$n)
data_50_sgtf_between <- rep(serialdata_50_sgtf_between$serial, serialdata_50_sgtf_between$n)
data_50_nsgtf_between <- rep(serialdata_50_nsgtf_between$serial, serialdata_50_nsgtf_between$n)

data_51_sgtf_within <- rep(serialdata_51_sgtf_within$serial, serialdata_51_sgtf_within$n)
data_51_nsgtf_within <- rep(serialdata_51_nsgtf_within$serial, serialdata_51_nsgtf_within$n)
data_51_sgtf_between <- rep(serialdata_51_sgtf_between$serial, serialdata_51_sgtf_between$n)
data_51_nsgtf_between <- rep(serialdata_51_nsgtf_between$serial, serialdata_51_nsgtf_between$n)

data_comb_sgtf_within <- rep(serialdata_comb_sgtf_within$serial, serialdata_comb_sgtf_within$n)
data_comb_nsgtf_within <- rep(serialdata_comb_nsgtf_within$serial, serialdata_comb_nsgtf_within$n)
data_comb_sgtf_between <- rep(serialdata_comb_sgtf_between$serial, serialdata_comb_sgtf_between$n)
data_comb_nsgtf_between <- rep(serialdata_comb_nsgtf_between$serial, serialdata_comb_nsgtf_between$n)
