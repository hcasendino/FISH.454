# Project 3 
# DI and DD models with process error. 


years <- 1983:2010
Ntobs <- grizzly$Est_Total_Females
# Get starting observed value for (1982). Choose similar starting value as 1983 (which is 41).


####====== 
# Density independent 

DI_proc <- function(pars, Ntobs){
  
  r <- pars[1]
  sigma <- pars[2]
  Nstart <- pars[3]
  epsilon_t <- rep(NA, length(Ntobs))
 
  epsilon_t[1] <- Ntobs[1] - (Nstart + r*Nstart)

      for(i in 1:(length(Ntobs)-1)){
        epsilon_t[i+1] <- Ntobs[i+1] - (Ntobs[i] + r*Ntobs[i])
      }

  nll<- -sum(dnorm(x = epsilon_t, mean = 0, sd = sigma, log = T), na.rm =T)
  return(nll)
}

start.pars <- c(0.0001, 0.01, 25)
dind.soln <-  optim(start.pars,
                    fn=DI_proc,
                    method = "Nelder-Mead",
                    Ntobs = Ntobs)
dind.soln$value
# nll: 103.4858

##====
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

NT <- c(22.26034457, Nt.hat.di)
NT <- NT[-29]
NT_plus <- Nt.hat.di

ntob<- c(22.26034457, Ntobs)
ntob <- ntob[-29]
ntob_plus <- Ntobs

plot(NT, NT_plus, col="green", type="l", xlab="N_t", ylab="N_t+1", xlim=c(0,140), ylim=c(0,140))
points(ntob, ntob_plus,  pch=16)
legend("bottomright", legend=c("Density Independent"), col="green", lwd=2, 
       cex=.6)

####====== 
# Density dependent 


DD_proc <- function(pars, Ntobs){
  r <- pars[1]
  K <- pars[2]
  sigma <- pars[3]
  Nstart <- pars[4]
  epsilon_t <- rep(NA, length(Ntobs))
  
  epsilon_t[1] <- Ntobs[1] - (Nstart + r*Nstart*(1 - Nstart/K))
  
  for(i in 1:(length(Ntobs)-1)){
    epsilon_t[i+1] <- Ntobs[i+1] - (Ntobs[i] + r*Ntobs[i]*(1 - Ntobs[i]/K))
  }
  
  nll<- -sum(dnorm(x = epsilon_t, mean = 0, sd = sigma, log = T), na.rm =T)
  return(nll)
}

start.pars <- c(0.0001, 120, 0.01, 25) 
dd.soln <-  optim(start.pars,
                    fn=DD_proc,
                    method = "Nelder-Mead",
                    Ntobs = Ntobs)
dd.soln$value

# nll: 100.7152


dd.nhat <- function(pars, Ntobs) {
  r <- pars[1]
  K <- pars[2]
  sigma <- pars[3]
  Nstart <- pars[4]
  
  Nt <- rep(NA, length(Ntobs))
  
  Nt[1]<- Nstart + r*Nstart*(1 - Nstart/K)
  
  for(i in 1:(length(Ntobs)-1)){
    Nt[i+1] <- Nt[i] + r*Nt[i]*(1 - Nt[i]/K)
  }
  return(Nt)
}

Nt.hat.dd <- dd.nhat(dd.soln$par, Ntobs)

#####
NT <- c(38.0251347, Nt.hat.dd)
NT <- NT[-29]
NT_plus <- Nt.hat.dd

ntob<- c(38.0251347, Ntobs)
ntob <- ntob[-29]
ntob_plus <- Ntobs

plot(NT, NT_plus, col="red", type="l", xlab="N_t", ylab="N_t+1", xlim=c(0,140), ylim=c(0,140))
points(ntob, ntob_plus, pch=16)
legend("bottomright", legend=c("Density Dependent"), col="red", lwd=2, 
       cex=.6)