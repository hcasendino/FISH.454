#  Below I have sketched out code for you.  Code comments marked with ### indicate where you
#  have to write your own code, following the instructions

# killed = q*effort*pop.size. Testing whether q changed between two years

# first, the data
killed <- c(79, 70)
pop.size <- c(1024, 990)
effort <- c(0.267, 0.213)



# function to return negative log-likelihood from single q model
singleq.nll <- function(par, killed, pop.size, effort) {
  q <- par[1]
  probs=effort * q
  nll <- -sum(dbinom(killed, prob = probs, size = pop.size, log = TRUE))
  return(nll)
}




# function to return negative log-likelihood from two q model
twoq.nll <- function(par, killed, pop.size, effort) {
  q <- par[1]
  deltaq <- par[2]
  probs=effort * c(q,q+deltaq)
  nll <- -sum(dbinom(killed, prob = probs, size = pop.size, log = TRUE))
  return(nll)
}



# fit single q model
start.par <- 0.3
single.fit <- optim(par = start.par,
                            fn = singleq.nll,
                            killed = killed, 
                            pop.size = pop.size, 
                            effort = effort,
                            method = "Brent",
                            lower = 0.01,
                            upper = 0.99)


# fit two q model
start.par <- c(0.3, 0)
two.fit <- optim(par=start.par, 
                fn=twoq.nll,
                killed = killed, 
                pop.size = pop.size, 
                effort = effort,
                method = "Nelder-Mead")
  
nll.single <- single.fit$value # Negative log-likelihood from single q model:  6.456469
nll.two <- two.fit$value # Negative log-likelihood from two q model: 6.072298



# Create AIC table
AIC <- matrix(nrow = 2, ncol = 2)
rownames(AIC) <- c("single q", "two q's")
colnames(AIC) <- c("AIC", "Delta AIC") 
# POPULATE AIC table with AIC in first column, delta AIC in the second column
AIC[1,1] <- 2*( 6.456469 + 3)
AIC[2,1] <- 2*( 6.072298 + 4)
AIC[,2] <- AIC[,1] - min(AIC[,1])  # this calculates delta AIC
AIC # print the output