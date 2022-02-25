source("simfun.R")
load("fit_lognormal_base.rda")

nsim <- 2000

kvec <- c(0.1, 1, 10)
householdvec <- c(4, 8, 12, 16, 20)
ratiovec <- c(1.5, 2, 2.5)
pardata <- expand.grid(kvec, householdvec, ratiovec)

R_delta <- 0.8

logmean <- coef(fit_lognormal_base_50_nsgtf_within)[1]
logsd <- coef(fit_lognormal_base_50_nsgtf_within)[2]

reslist <- vector('list', nrow(pardata))

set.seed(101)
for (i in 1:nrow(pardata)) {
  print(i)
  delta_egocentric <- unlist(replicate(nsim, simfun_egocentric(R=R_delta, k=pardata[i,1], logmean, logsd, household=pardata[i,2])))
  delta_house <- unlist(replicate(nsim, simfun_house(R=R_delta, k=pardata[i,1], logmean, logsd, household=pardata[i,2])))
  
  omicron_egocentric <- unlist(replicate(nsim, simfun_egocentric(R=R_delta*pardata[i,3], k=pardata[i,1], logmean, logsd, household=pardata[i,2])))
  omicron_house <- unlist(replicate(nsim, simfun_house(R=R_delta*pardata[i,3], k=pardata[i,1], logmean, logsd, household=pardata[i,2])))
  
  reslist[[i]] <- data.frame(
    mean=c(mean(delta_egocentric), mean(delta_house), mean(omicron_egocentric), mean(omicron_house)),
    lwr=c(
      t.test(delta_egocentric)[[4]][[1]],
      t.test(delta_house)[[4]][[1]],
      t.test(omicron_egocentric)[[4]][[1]],
      t.test(omicron_house)[[4]][[1]]
    ),
    upr=c(
      t.test(delta_egocentric)[[4]][[2]],
      t.test(delta_house)[[4]][[2]],
      t.test(omicron_egocentric)[[4]][[2]],
      t.test(omicron_house)[[4]][[2]]
    ),
    strain=c("Delta", "Delta", "Omicron", "Omicron"),
    type=c("Egocentric", "Household", "Egocentric", "Household"),
    k=pardata[i,1],
    household=pardata[i,2],
    ratio=pardata[i,3]
  )
}

simulate_generation_household <- reslist %>%
  bind_rows

save("simulate_generation_household", file="simulate_generation_household.rda")
