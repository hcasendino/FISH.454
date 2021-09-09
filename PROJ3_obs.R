# Project 3 
# DI and DD models with observation error. 


years <- 1983:2010
Ntobs <- grizzly$Est_Total_Females
# Get starting observed value for (1982). Choose similar starting value as 1983 (which is 41).



####========
# Density Independent

DI_obs <- function(pars, Ntobs){

  Nt <- rep(NA, length(Ntobs))
  r <- pars[1]
  sigma <- pars[2]
  Nstart <- pars[3]
  
  Nt[1]<- Nstart + r*Nstart
  
  for(i in 1:(length(Ntobs)-1)){
    Nt[i+1] <- Nt[i] + r*Nt[i]
  }
  
  epsilon_t <- Ntobs - Nt
  nll<- - sum(dnorm(x = epsilon_t, mean = 0, sd = sigma, log = T), na.rm = TRUE)
  return(nll)
}

# Get values of r and sigma that make epsilons most likely, and the nll of those params: 110.0014
start.pars <- c(0.1, 1, 25) 
dind.soln <-  optim(start.pars,
                     fn=DI_obs,
                     method = "Nelder-Mead",
                     Ntobs = Ntobs)

# DO THIS 
di.nhat <- function(pars, Ntobs) {
  r <- pars[1]
  sigma <- pars[2]
  Nstart <- pars[3]
  
  Nt <- rep(NA, length(Ntobs))
  
  Nt[1]<- Nstart + r*Nstart
  
  for(i in 1:(length(Ntobs)-1)){
    Nt[i+1] <- Nt[i] + r*Nt[i]
  }
  return(Nt)
}

Nt.hat.di <- di.nhat(dind.soln$par, Ntobs)


plot(years, Ntobs, pch=16, xlab="Year", ylab="Number of Grizzlies", ylim=c(0,142))
lines(years,Nt.hat.di, col="green", type="l")
legend("topleft", legend=c("Density Independent"), col="green", lwd=2, 
       cex=.6)
  

####=========
# Density Dependent 

DD_obs<- function(pars, Ntobs) {
  r <- pars[1]
  K <- pars[2]
  sigma <- pars[3]
  Nstart <- pars[4]
  
  Nt <- rep(NA, length(Ntobs))
  Nt[1]<- Nstart + r*Nstart*(1 - Nstart/K)
  
  for (i in 1:(length(Ntobs)-1)){
    Nt[i+1]<- Nt[i] + r*Nt[i]*(1 - Nt[i]/K)
  }
  epsilon_t <- Ntobs - Nt
  nll<- - sum(dnorm(x = epsilon_t, mean = 0, sd = sigma, log = T), na.rm = TRUE)
  return(nll)
}

# Get values of r, K, and sigma that make epsilons most likely, and the nll of those params : 101.553
start.pars <- c(0.1, 120, 1, 25) 
ddep.soln <-  optim(start.pars,
                     fn=DD_obs,
                     method = "Nelder-Mead",
                     Ntobs = Ntobs)

dd.nhat <- function(pars, Ntobs) {
  r <- pars[1]
  K <- pars[2]
  sigma <- pars[3]
  Nstart <- pars[4]
  
  Nt <- rep(NA, length(Ntobs))
  
  Nt[1]<- Nstart + r*Nstart*(1 - Nstart/K)
  
  for (i in 1:(length(Ntobs)-1)){
    Nt[i+1]<- Nt[i] + r*Nt[i]*(1 - Nt[i]/K)
  }
  return(Nt)
}

Nt.hat.dd <- dd.nhat(ddep.soln$par, Ntobs)


plot(years, Ntobs, pch=16, xlab="Year", ylab="Number of Grizzlies", ylim=c(0,142))
lines(years,Nt.hat.dd, col="red", type="l")
legend("topleft", legend=c("Density Dependent"), col="red", lwd=2, 
       cex=.6)


