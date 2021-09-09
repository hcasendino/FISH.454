## Likelihood Lab Code 


########=============
# QUESTION 1 

p<- seq(0.05,0.7,0.025)
x<- 3
likelihood_mat <- matrix(NA, nrow=length(p), ncol=1)

for(i in 1:length(p)){
  a <- dbinom(x, prob = p[i],  size = 10, log = TRUE)
  likelihood_mat[i]<- a
}

# For each r, sum the negative log likelihoods of each observations
neglik<-(likelihood_mat)*-1

plot(p,neglik, xlab = "predation probability",
     ylab= "negative log-lik", type="l", lwd=2.5, ylim=c(0,5))

# Find r with smallest negative log likelihood
p[which(neglik==min(neglik))]
# p = 0.3

# Add 1.92 to get upper and lower limits of r
min(neglik) + 1.92
# 3.241151

p[3]
p[23]
#####=================
# Question 2: 

x <- c(3,4,10)
trials <- c(10, 15, 25)
p<- seq(0.05,0.7,0.025)
mat<- matrix(NA, nrow=length(p), ncol=3)

for(i in 1:length(p)){
  p_binom<- p[i]
  
  for(j in 1:length(x)){
    x_binom <- x[j]
    size_binom <- trials[j]
    a <- dbinom(x_binom, prob = p_binom, size = size_binom , log = TRUE)
    mat[i,j]<- a
  }
}

row_sums<-rowSums(mat)*-1
p[which(row_sums==min(row_sums))]
# 0.35

plot(p,row_sums, xlab = "predation probability",
     ylab= "negative log-lik", type="l", lwd=2.5, ylim=c(0,26))

min(row_sums) + 1.92
# 6.97649

p[8]
p[18]

#####=================
# Question 3: 

binom.nll <- function(par, thedata, trials) {
  p <- par
  nll <- -sum(dbinom(thedata, 
                             prob = p, 
                             size = trials, 
                             log = TRUE))
  return(nll)
}

p.start <- 0.3
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


#####=================
# Question 4:

N_dan<- c(46, 44, 79, 39, 37)
t<- 20 
 
nll.fun<- function(pars, N_dan, t){
  r<- pars[1]
  k<- pars[2]
  nll<- -sum(dnbinom(N_dan, size = k, mu = r*t, log=T))
  return(nll)
}

start.pars <- c(0.01, 10)

nb.soln<- optim(par=start.pars, 
                fn=nll.fun,
                t = t,
                N_dan = N_dan,
                method = "Nelder-Mead")

print((nb.soln$par))

### Maximum likelihood estimation of r: 2.45
### Maximum likelihood estimation of k: 15.75

