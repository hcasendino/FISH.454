read.table(file="dat_df1.dat", header=T)



# Starting population size and population growth rate
Nt<- 1
lambda<- 1.3

#Vector to hold population sizes through time series (10 years)
pop_sizes<- rep(x= NA, times= 10)

#Loop projecting pop size 10 years in
for(i in 1:10){
  Nt<- Nt*lambda
  pop_sizes[i]<- Nt
}
pop_sizes

#Larger loop
N_0 <- 1
lambda.list<- seq(.5,1.5,.1)
m<- matrix(data=NA, nrow=10, ncol=length(lambda.list))

for(i in 1:length(lambda.list)){
  lambda<- lambda.list[i]
  Nt<- 1
  pop_sizes<- rep(x= NA, times= 10) 
  pop_sizes[1]<-Nt
  for(j in 2:10){
    Nt<- Nt*lambda
    pop_sizes[j]<- Nt
  }
  m[,i]<- pop_sizes
}

#Creating a plot of the relationship between N10 vs. λ when N_0 = 1
m<- as.data.frame(m)
colnames(m) <- c(seq(.5,1.5,.1))
m<- m[10,]
m<- t(m)
m<- as.data.frame(m)
m <- cbind(rownames(m), data.frame(m, row.names=NULL)) 
colnames(m) <- c("lambda", "N10")
plot(m, lwd=2)


