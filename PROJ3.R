# code to make AIC table
AIC <- matrix(nrow = 4, ncol = 4)
rownames(AIC) <- c("DI Obs", "DD Obs", "DI Proc", "DD Proc")
colnames(AIC) <- c("# Params", "NLL", "AIC", "Delta AIC") 
AIC[1,1] <-  3
AIC[2,1] <-4 
AIC[3,1] <- 3 
AIC[4,1] <- 4
AIC[1,2] <- 108.2544
AIC[2,2] <-101.553 
AIC[3,2] <- 103.4858  
AIC[4,2] <- 100.7152
AIC[1,3] <- 2 * (108.2544 + 3)
AIC[2,3] <-2 * (101.553 + 4)
AIC[3,3] <- 2 * (103.4858 + 3)
AIC[4,3] <-2 * (100.7152 + 4 )
AIC[,4] <- AIC[,3] - min(AIC[,3])  # this calculates delta AIC
AIC # print the output



# r CONFIDENCE INTERVAL: 


r.prof <- function(pars, Ntobs, r){
  
  K<- pars[1]
  sigma <- pars[2]
  Nstart <- pars[3]

  epsilon_t <- rep(NA, length(Ntobs))
  epsilon_t[1] <- abs(Ntobs[1] - (Nstart + r*Nstart*(1 - Nstart/K)))
  
  for(i in 1:(length(Ntobs)-1)){
    epsilon_t[i+1] <- abs(Ntobs[i+1] - (Ntobs[i] + r*Ntobs[i]*(1 - Ntobs[i]/K)))
  }
  nll<- -sum(dnorm(x = epsilon_t, mean = 0, sd = sigma, log = T), na.rm =T)
  return(nll)
}
start.pars<- dd.soln$par[2:4]


rlist<- seq(0.01,0.4, 0.0005)
r.profile <- rep(NA, length(rlist))

for( i in 1:length(rlist)){
  
start.pars<- c(121.922935, 5, 38.025135) # why can't I use MLE sigma? 
soln.profile <- optim(start.pars, 
                      fn = r.prof,
                      method= "Nelder-Mead",
                      Ntobs= Ntobs,
                      r = rlist[i]
                      )
r.profile[i] <- soln.profile$value
}

plot(rlist, r.profile, type = "l", col="purple", lwd=2, 
     xlab= "r Value", ylab="Profile Negative Log Likelihood")
abline(h = 101.8989, lty=2)

min(r.profile) + 1.92 #  101.8989
rlist[15]  # 0.017
rlist[589] # 0.304
rlist[300]
