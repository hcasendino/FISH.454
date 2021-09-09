# Project 2 Code (Predator Handling Model)
# FISH 454
# Helen Casendino

####=============================
# Dependencies 
library(formattable)

####=============================
# Base Model: Parameters

a <- 0.2 
b <- 0.15 
c <- 0.3
d <- 0.15 
K1 <- 10
K2 <- 20
h <- 0.5 

# K table
Ktab<- data.frame(K= c("ten", "twenty"), "N_star" = c(4.44,4.44), 
                  "P_star" = c(0.99,1.38))
formattable(Ktab)

####=============================

# Plot predator (red) and prey (blue) isoclines. x=N

curve((a/b)*(1 + b*h*x - (x/K2) - ((b*h*x^2)/K2)), from=0, to=20,
      lwd=3, col="blue", xlab="Prey", ylab="Predator",  xaxs="i",yaxs="i",
      ylim=c(0,20))
abline(v=-d/(b*(d*h-c)), lwd=3, col="red")
      
# Get equilibrium values for prey and predator. 
Nstar<- -d/(b*(d*h-c))
Pstar<- (a/b)*(1 + b*h*Nstar - (Nstar/K2) - ((b*h*Nstar^2)/K2))


# Graph model trajectory when population sizes are 10%. 
N.output<-matrix(NA,nrow=150,ncol=3)

N.output[1,1:3]<-c(1,Nstar*1.1,Pstar*1.1)

for (i in 2:150){
  N<-N.output[i-1,2]
  P<-N.output[i-1,3]
  N.output[i,2]<- N + a*N*(1-(N/K2)) - (b*N*P)/(1+b*h*N)
  N.output[i,3]<- P + (c*b*N*P)/(1+b*h*N) - d*P
  N.output[i,1]<- i
}
lines(N.output[,2],N.output[,3],col="black",lwd=2)


# Graph population abundance overtime 
matplot(N.output[,1], N.output[,c(2,3)], col=c("blue","red"),
        type="l", lwd=2, lty=1, xlab="Year", ylab="Population Density", ylim=c(0,10))
legend(x="topleft", legend=c("Prey", "Predator"), lty=1, col=c("blue", "red"),
       bty = "n")


# Create Jacobian Matrix  & max magnitude of eigen 
f_N<- 1 + a - ((2*a*Nstar)/K2) - (b*Pstar)/((1 + b*h*Nstar)^2)
f_P<- (-b*Nstar)/(1 + b*h*Nstar)
g_N<- (c*b*Pstar)/((1 + b*h*Nstar)^2)
g_P<- 1 + ((c*b*Nstar)/(1 + b*h*Nstar)) - d

A<- matrix(data=c(f_N,g_N,f_P,g_P), nrow=2, ncol=2)

ev<- eigen(A)$values
mags<- sqrt(Re(ev)^2 + Im(ev)^2)
max.eigen <- max(mags)

      
