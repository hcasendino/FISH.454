#  Below I have sketched out code for you.  Code comments marked with ### indicate where you
#  have to write your own code, following the instructions

# The data: 
Ntobs <- c(2570,
           NA,
           2864,
           4408,
           5197,
           4416,
           4203,
           6008,
           4807,
           7600,
           6796,
           6475,
           NA,
           8681,
           7761,
           8161,
           5786,
           6492,
           7191,
           7634,
           NA,
           7117)

years <- 1978:1999

# a function for caclulating negative log likelihood for density independent model
density.independent.nll <- function(pars, Ntobs) {
  r <- pars[1]
  N1978 <- pars[2]
  sigma <- pars[3]
  ndata <- length(Ntobs)
  Nt <- rep(NA, ndata)
  Nt[1] <- N1978
  
  for(i in 1:21){
    Nt[i+1] <- Nt[i] + r*Nt[i]
  }
  
  epsilon_t <- Ntobs - Nt
  nll<- - sum(dnorm(x = epsilon_t, mean = 0, sd = sigma, log = T), na.rm = TRUE)
  return(nll)
}

# a function for caclulating negative log likelihood for logistic model
logistic.nll<- function(pars, Ntobs) {
  r <- pars[1]
  K <- pars[2]
  N1978 <- pars[3]
  sigma <- pars[4]
  
  ndata <- length(Ntobs)
  Nt <- rep(NA, ndata)
  Nt[1] <- N1978
  
  
  for(i in 1:21){
    Nt[i+1] <- Nt[i] + r*Nt[i]*(1 - Nt[i]/K)
  }
  
  epsilon_t <- Ntobs - Nt
  nll<- - sum(dnorm(x = epsilon_t, mean = 0, sd = sigma, log = T), na.rm = TRUE)
  return(nll)
}


# fit density independent model
start.pars <- c(0.1, 3000, 400) 
ddind.soln <-  optim(start.pars,
                      fn=density.independent.nll,
                     method = "Nelder-Mead",
                     Ntobs = Ntobs)

ddind.nll <- ddind.soln$value # negative log likelihood: 160.6871

# estimate parameters of logistic model
start.pars <- c(0.1, 7000, 3000, 400) 
logistic.soln <- optim(start.pars,
                       fn=logistic.nll,
                       method = "Nelder-Mead",
                       Ntobs = Ntobs)
              
logistic.nll <- logistic.soln$value # negative log likelihood: 154.6046


# code to make AIC table
AIC <- matrix(nrow = 2, ncol = 2)
rownames(AIC) <- c("Density Independent", "Density Dependent")
colnames(AIC) <- c("AIC", "Delta AIC") 
AIC[1,1] <- 2 * (160.6871 + 2 )
AIC[2,1] <-2 * (154.6046 + 3 )
AIC[,2] <- AIC[,1] - min(AIC[,1])  # this calculates delta AIC
AIC # print the output



# function to calculate predicted Nt based on a set of density independent model parameters
di.nhat <- function(pars) {
  r <- pars[1]
  N1978 <- pars[2]
  sigma <- pars[3]
  ndata <- length(Ntobs)
  Nt <- rep(NA, ndata)
  Nt[1] <- N1978
  
  for(i in 1:21){
    Nt[i+1] <- Nt[i] + r*Nt[i]
  }
  return(Nt)
}

# function to calculate predicted Nt based on a set of logistic model parameters
logistic.nhat <-function(pars) {
  r <- pars[1]
  K <- pars[2]
  N1978 <- pars[3]
  sigma <- pars[4]
  
  ndata <- length(Ntobs)
  Nt <- rep(NA, ndata)
  Nt[1] <- N1978
  
  for(i in 1:21){
    Nt[i+1] <- Nt[i] + r*Nt[i]*(1 - Nt[i]/K)
  }
  return(Nt)
}

# this code gets predicted population size-at-time for each model at maximum likelihood parameter estimates
Nt.hat.di <- di.nhat(ddind.soln$par)
Nt.hat.logistic<- logistic.nhat(logistic.soln$par)

#### Plot data, and model fits
plot(years, Ntobs, pch=16, xlab="Year", ylab="Number of Harbor Seals")
lines(years,Nt.hat.di, col="green", type="l")
lines(years,Nt.hat.logistic, col="red", type="l")
legend(x="topleft", legend=c("density dependent", "density independent"), 
       col=c("red", "green"), lwd=3, cex=0.7)

