# Project 2 Code (Prey Density Dependence Model)
# FISH 454
# Helen Casendino

####=============================
# Base Model: Parameters

a <- 0.2 
b <- 0.15 
c <- 0.3
d <- 0.15 
K <- 20 

####=============================
# Model:

#Original plot 
plot(0, type='n', xlim=c(0,17), ylim=c(0,17), 
     xlab="Prey", ylab="Predator", yaxs="i", xaxs="i")

# Plot predator (red) and prey (blue) isoclines
abline(v= d/(c*b), col="red", lwd=3)
points(c(0,15), c(a/b, 0), type="l", col="blue", lwd=3)


# Get equilibrium values for prey and predator. 
Nstar<- d/(c*b)
Pstar<- (-a*Nstar)/(b*K) + a/b


# Graph model trajectory when population sizes are 10%. 
N.output<-matrix(NA,nrow=150,ncol=3)

N.output[1,1:3]<-c(1,Nstar*1.1,Pstar*1.1)

for (i in 2:150){
  N<-N.output[i-1,2]
  P<-N.output[i-1,3]
  N.output[i,2]<- N + a*N*(1-(N/K)) - b*N*P
  N.output[i,3]<- P + c*b*N*P - d*P
  N.output[i,1]<- i
}
lines(N.output[,2],N.output[,3],col="black",lwd=2)


# Graph population abundance overtime 
matplot(N.output[,1], N.output[,c(2,3)], col=c("blue","red"),
        type="l", lwd=2, lty=1, xlab="Year", ylab="Population Density", ylim=c(0,10))
legend(x="topleft", legend=c("Prey", "Predator"), lty=1, col=c("blue", "red"),
       bty = "n")


# Create Jacobian Matrix  & max magnitude of eigen 
f_N<- 1 + a - ((2*a*Nstar)/K) - b*Pstar
f_P<- -1*b*Nstar
g_N<- c*b*Pstar
g_P<- 1 + (c*b*Nstar) - d

A<- matrix(data=c(f_N,g_N,f_P,g_P), nrow=2, ncol=2)

ev<- eigen(A)$values
mags<- sqrt(Re(ev)^2 + Im(ev)^2)
max.eigen <- max(mags)
