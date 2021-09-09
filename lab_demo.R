thedata <- c(3,4,10)
trials <- c(10, 15, 25)

binom.nll <- function(par, thedata, trials) {
  p <- par # this looks silly for now, makes sense when there are multiple parameters
  # calculate negative log likelihood (nll)
  nll <- 0
  # loop through observations, adding negative log likelihood of each observation 
  #  to the running sum
  for (i in 1:length(thedata)) nll <- nll - dbinom(thedata[i], 
                                                   prob = p, 
                                                   size = trials[i], 
                                                   log = TRUE)
  # or, do it all in one line!
  nll <- -sum(dbinom(thedata, 
                     prob = p, 
                     size = trials, 
                     log = TRUE))
  return(nll)
}

# create starting value
p.start <- 0.25
# call optim
soln <- optim(par = p.start,
              fn = binom.nll,
              thedata = thedata,
              trials = trials,
              method = "Brent",
              lower = 0.01,
              upper = 0.99)
p.mle <- soln$par
min.nll <- soln$value

##### Negative Binomial Example
thedata <- c(46, 44, 79, 39, 37)
t <- 20

negbin.fun <- function(par, thedata, t) {
  r <- par[1] # the first element of array "par" is r
  n <- par[2] # the second element of array "par" is n
  return(-sum(dnbinom(thedata, mu = r*t, size = n, log = T)))
  
}

start.par <- c(2, 10) # starting values
negbin.solve <- optim(par = start.par,
                      fn = negbin.fun,
                      thedata = thedata,
                      t = t,
                      method = "Nelder-Mead"
)

  