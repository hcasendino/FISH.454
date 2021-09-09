P.egg <- 0.75
G.egg<- 0.25
P.larvae <- 0.14
G.larvae <- 0.00032
P.juvenile <- 0.82
G.juvenile<- 0.00154
P.adult <- 0.975
F.adult <- 9000

# Create a blank A matrix full of zeros
A <- matrix (0, nrow = 4, ncol = 4)

# insert parameters above as needed
A[1,]<- c(0,0,0,F.adult)
A[2,]<- c(G.egg,P.larvae,0,0)
A[3,]<- c(0,G.larvae,P.juvenile,0)
A[4,]<- c(0,0, G.juvenile,P.adult)
 
# get population growth rate (per month) using the eigen command.  Eigen will return eigenvalues and eigen vectors.  to get just the values, use eigenvalues <- eigen(A)$values
eigen(A)
#lambda<- 0.983195525


### POPULATION DYNAMICS OVER 50 YEARS

egg.start <- 20
larvae.start <- 15
juv.start <- 10
adult.start <- 5


tmax<-50
years <- 0:tmax

output <- matrix (NA, nrow = length(years), ncol = 4)
colnames(output) <- c("Eggs", "Larvae", "Juvenile", "Adult")
output[1,]<- c(egg.start, larvae.start, juv.start, adult.start)

for(i in 1:tmax){
  output[i+1,]<- A %*% output[i,]
}

# population yearly growth rate checks out = 0.9831947
sum(as.vector(output[51,]))/sum(as.vector(output[50,]))

#plot
col<- plasma(n=5)

matplot(years, 
        output, 
        col=yarrr::transparent(col, trans.val = .4) , 
        xlab="year", 
        ylab="abundance", 
       type="l", lwd=5)
legend(x="topright", bg = "white",
       col=col, lwd=5, legend=c("eggs","larvae","juvenile", "adult"))

