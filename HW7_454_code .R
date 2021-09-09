#### Homework 7

tadpoles<- read.csv("TadpolesData.csv")

binom.nll <- function(pars, n_killed, n_start) {
  a<- pars[1]
  h<- pars[2]
  nll <- -sum(dbinom(n_killed, 
                     prob = a/(1 + a*h*n_start), 
                     size = n_start, 
                     log = TRUE))
  return(nll)
}

start.pars<- c(0.2, 0.01)
  
# call optim
soln <- optim(par = start.pars,
              fn = binom.nll,
              n_killed = tadpoles$Killed,
              n_start = tadpoles$InitialNumber,
              method = "Nelder-Mead")
mle <- soln$par
### MLE of a: 0.53. MLE of h: 0.02

min.nll <- soln$value
### MLE Neg log likelihood: 46.72


########## Confidence interval / Likelihood profile

r.b.profile <- function(n_start, n_killed){
  nll<- rep(NA, times=length(n_start))
  for (i in 1:length(n_start)){
    nll[i] <- -dbinom(n_killed[i], n_start[i], 
                    prob = a/(1 + a*h*n_start[i]), log=T)
  }
  return(sum(nll))
}

alist<- seq(0.2, 0.7, 0.01)
hlist<- seq(0.005, 0.03, 0.00025)

nll<- matrix(data=NA, nrow=length(alist), ncol=length(hlist))

for(i in 1:length(alist)){
  a<- alist[i]
  for(j in 1:length(hlist)){
    h<- hlist[j]
    nll[i,j]<- r.b.profile(n_start=tadpoles$InitialNumber, n_killed= tadpoles$Killed)
  }
}

a.profile <- apply(X=nll, MARGIN=1, FUN=min)
min(a.profile)
alist[34]
### MLE of a:  0.53 

min(a.profile) + 1.92
# 48.64348
alist[22]
alist[49]
### 95% confidence interval between a = 0.41 and 0.68

h.profile <- apply(X=nll, MARGIN=2, FUN=min)
min(h.profile)
hlist[48]
### MLE of h: 0.02

min(h.profile) + 1.92
# 48.64348
hlist[10]
hlist[86]
### 95% confidence interval between h = 0.0073 and 0.026




########## PLOT 
problist<- rep(NA, length.out=length(tadpoles$Trial))

plot(tadpoles$InitialNumber, tadpoles$Killed, pch=16,
     xlab="Initial Number of Tadpoles", ylab="Tadpoles Killed")

for(i in 1:length(tadpoles$Trial)){
  n_start<- tadpoles$InitialNumber[i]
  problist[i] <-  0.53/(1 + 0.53*0.02*n_start)
}

predicted_kill<- problist*tadpoles$InitialNumber

lines(tadpoles$InitialNumber, predicted_kill, type="l", col="red")

legend(x="topleft", legend=c("h = 0.02"), 
       col=c("red"), lwd=3, cex=0.7)

