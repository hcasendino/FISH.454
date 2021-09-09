
# Q1B

N<- 20
x <- 0:20
p<- 0.5
prob_vect<- rep(NA, length=21)

for(i in 0:20){
  prob <- dbinom(x[i], size = N, prob = p)
  prob_vect[i]<- prob

  if(i==20){
  prob_vect[21]<- prob
  }
}
mortality_rate <- x/20
sum(prob_vect[which(mortality_rate > 0.4 & mortality_rate < 0.6)])

#  The probability that we will get an observed mortality rate 
#   that is within 0.1 of the true (0.5) mortality rate is 0.4965553.



# Q1C

N<- 20
x <- 0:20
p<- 0.25
prob_vect<- rep(NA, length=21)
for(i in 0:20){
  prob <- dbinom(x[i], size = N, prob = p)
  prob_vect[i]<- prob
  if(i==20){
    prob_vect[21]<- prob
  }
}
mortality_rate <- x/20
sum(prob_vect[which(mortality_rate > 0.15 & mortality_rate < 0.35)])


N<- 20
x <- 0:20
p<- 0.75
prob_vect<- rep(NA, length=21)
for(i in 0:20){
  prob <- dbinom(x[i], size = N, prob = p)
  prob_vect[i]<- prob
  if(i==20){
    prob_vect[21]<- prob
  }
}
mortality_rate <- x/20
sum(prob_vect[which(mortality_rate > 0.65 & mortality_rate < 0.85)])



# Q2A 

dpois(51, lambda = 0.14 * 194.4)

#Q 2B 

x<-c(0:100)
xvec<- rep(NA, times=length(x))

for(i in 1:length(x)){
  a<- x[i]
  b<- dpois(a, lambda = 0.14 * 194.4)
  xvec[i]<- b
}

plot(x,xvec, type="l",xlab="outcome", ylab="probability")

x[which(xvec==max(xvec))]
27/194.4 
# Close to 0.14 


# Q 2C
sum(xvec[52:101])

# Q 3A

x<- c(0:100)
xvec<- rep(NA, times=length(x))

for(i in 1:length(x)){
  a<- x[i]
  b<- dnbinom(a, mu = 0.14 * 194.4, size = 1)
  xvec[i]<- b
}

plot(x,xvec, type="l",xlab="outcome", ylab="probability")

# Q 3B 
sum(xvec[52:101])
x[which(xvec==max(xvec))]

