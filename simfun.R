sample2 <- function(x, size) {
  if(length(x)==1) {
    rep(x, size)
  } else{
    sample(x, size, replace=TRUE)
  }
}

simfun_egocentric <- function(R=1.7,
                              k=0.1,
                              meanlog=1.2,
                              sdlog=0.5,
                              household=4) {
  z <- 0
  
  while (z == 0) {
    z <- rnbinom(1, mu=R, size=k)
  }
  
  gen <- sort(rlnorm(z, meanlog, sdlog))
  
  if (z < household) {
    return(gen)
  } else {
    return(gen[1:household])
  }
}

simfun_house <- function(R=1.7,
                         k=0.1,
                         meanlog=1.2,
                         sdlog=0.5,
                         household=4) {
  V <- 1:household
  
  initial_infected <- 1
  
  imax <- household
  
  queue_v <- queue_t <- queue_infector <- NA
  
  queue_v <- initial_infected 
  queue_t <- 0
  
  t_infected <- rep(NA, household)
  t_infected[initial_infected] <- 0
  
  t_gillespie <- NULL
  c_infected <- 0
  
  done <- rep(FALSE, household)
  infected_by <- rep(NA, household)
  
  stop <- FALSE
  
  while (!stop) {
    j.index <- which.min(queue_t)
    j <- queue_v[j.index]
    
    infected_by[j] <- queue_infector[j.index]
    t_infected[j] <- queue_t[j.index]
    
    t <- queue_t[j.index]; t_gillespie <- c(t_gillespie, t)
    
    c_infected <- c_infected +1
    
    n <- V[V != j]
    
    z <- rnbinom(1, mu=R, size=k)
    
    if (j==1) {
      ## force the first infection to have some infections to start an outbreak
      while (z == 0) {
        z <- rnbinom(1, mu=R, size=k)
      }
    }
    
    if (z > 0) {
      queue_v <- c(queue_v, sample2(n, z))
      queue_infector <- c(queue_infector, rep(j, z))
    }
    
    gen <- rlnorm(z, meanlog, sdlog)
    
    if (z > 0) {
      queue_t <- c(queue_t, t + gen)
    }
    
    done[j] <- TRUE
    
    filter2 <- !done[queue_v]
    queue_v <- queue_v[filter2]
    queue_infector <- queue_infector[filter2]
    queue_t <- queue_t[filter2]
    
    stop <- (c_infected == length(V) || all(done[queue_v]) || c_infected == imax)
  }
  
  gen <- t_infected - t_infected[infected_by]
  
  gen[!is.na(gen)]
}
