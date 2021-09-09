
#  Below I have sketched out code for you.  Code comments marked with ### indicate where you
#  have to write your own code, following the instructions

# The data, first the starting goby densities
n.init <- c(6,10,12,20,29,43,59,91,22,25,8,12,15,42,68)
# then, the final goby counts (the ones that survived)
n.final <- c(4,4,4,
             7,
             9,
             8,
             18,
             11,
             9,
             8,
             3,
             7,
             5,
             17,
             16)


# function to calculate and return negative log likelihood from density independent model
di.nll <- function(par, n.init, n.final) {
  p.max <- par[1]
  pbinom <- rep(NA, times=length(n.init))
  for(i in 1:length(n.init)){
   pbinom[i] <-  dbinom(n.final[i], prob = pmax, size = n.init[i], log = TRUE)
  }
  nll <- -sum(pbinom)
  return(nll)
}

# function to calculate and return negative log likelihood from ricker model
ricker.nll <- function(par, n.init, n.final) {
  p.max <- par[1]
  alpha <- par[2]
  pbinom <- rep(NA, times=length(n.init))
   for(i in 1:length(n.init)){
       pbinom[i] <- dbinom(n.final[i], prob = p.max * exp(-alpha*n.init[i]), size = n.init[i], log = TRUE)
   }
  nll <- -sum(pbinom)
  return(nll)
}

bevertonholt.nll <- function(par, n.init, n.final) {
  p.max <- par[1]
  alpha <-par[2]
  pbinom <- rep(NA, times=length(n.init))
  for(i in 1:length(n.init)){
    pbinom[i] <- dbinom(n.final[i], prob = p.max/(1 + (alpha*n.init[i])), size = n.init[i], log = TRUE)
  }
  nll <- -sum(pbinom)
  return(nll)
}

# fit density independent
start.par <- 0.7
di.fit <- optim(par = start.par,
                fn = di.nll,
                n.init = n.init, 
                n.final= n.final,
                method = "Brent",
                lower = 0.01,
                upper = 0.99)
di.fit$value # Negative log-likelihood:  41.6192



# fit ricker
start.par <- c(0.7, 0.01)
ricker.fit <- optim(par=start.par, 
                    fn=ricker.nll,
                    n.init = n.init, 
                    n.final= n.final,
                    method = "Nelder-Mead")
ricker.fit$value # Negative log-likelihood:31.34186

# fit beverton-holt
start.par <- c(0.5, 0.1)
bevertonholt.fit <-optim(par=start.par, 
                         fn=bevertonholt.nll,
                         n.init = n.init, 
                         n.final= n.final,
                         method = "Nelder-Mead")
bevertonholt.fit$value # Negative log-likelihood: 31.87617

# Create AIC Table
  
AIC <- matrix(NA, nrow = 3, ncol = 2)
rownames(AIC) <- c("Density Independent", "Ricker", "Beverton Holt")
colnames(AIC) <- c("AIC", "DeltaAIC")
AIC[1,] <- 2 * (41.6192 + 2)
AIC[2,] <- 2 * (31.34186 + 3)
AIC[3,] <- 2 * (31.87617 + 3)
AIC[,2] <- AIC[,1] - min(AIC[,1]) # calculates delta AIC
AIC

# code for plotting expected number of survivors
n.list <- seq(from = 0, to = 91) # just a vector of initial densities

n.di <- di.fit$par * n.list
n.rickert <- ricker.fit$par[1] * n.list * exp(-ricker.fit$par[2] * n.list)
n.bh <- bevertonholt.fit$par[1] * n.list / (1 + bevertonholt.fit$par[2]*n.list)

### Plot the data (survived vs. initial density) as points, and fitted functions as lines

plot(n.init, n.final, pch=16,
     xlab="Initial Density", ylab="Dolphins Survived")

lines(n.list, n.di, type="l", col="red")
lines(n.list, n.rickert, type="l", col="blue")
lines(n.list, n.bh, type="l", col="green")

legend(x="topleft", legend=c("density independent", "ricker", "beverton-holt"), 
       col=c("red", "blue", "green"), lwd=3, cex=0.4)

