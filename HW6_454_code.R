### Likelihood Homework 

# Question 1: 
####=======================

geodata<- read.csv("Hood_canal_geoducks.csv")


nll.fun<- function(pars, n_counted, area){
  r<- pars[1]
  k<- pars[2]
  nll<- -sum(dnbinom(n_counted, size = k, mu = r*area, log=T))
  return(nll)
}

start.pars <- c(0.01, 10)

nb.soln<- optim(par=start.pars, 
                fn=nll.fun,
                area = geodata$Area_Surveyed,
                n_counted = geodata$Number_Counted,
                method = "Nelder-Mead")

print((nb.soln$par))

#### MLE of r: 0.21
#### MLE of k: 0.26


# Question 2: 
####========================

(nb.soln$value) + 1.92
# 240.8075

# r underlying density of geoduck 
# small k, high variance 
r<- seq(0.1, 0.4, length.out=100) 
k<- 0.26 
likelihood_mat <- matrix(NA, nrow=length(r), ncol=1)

for(i in 1:length(r)){
  a <- dnbinom(geodata$Number_Counted, size = k,
               mu = r[i]*geodata$Area_Surveyed, log=T)
  b<- sum(a)
  likelihood_mat[i]<- b*-1
}
likelihood_mat

r[12]
r[88]

# Confidence interval: r = 0.13 to r = 0.36
