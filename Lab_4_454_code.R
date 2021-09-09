### LAB 4 (454)


## Q1

output<- rep(x=NA, times=1000)
for(j in 1:1000){
  
  rbar<- 0.0016
  sd<- 0.038
  N0<- 79
  tmax<- 100
  Nextinct<- 40
  years<- 0:tmax
  Nt<- rep(x=NA, times=tmax)
  
  rt<- rnorm(n=length(years), mean=rbar, sd=sd)
  Nt[1]<- N0
  for(i in 1:tmax){
    Nt[i+1]<- exp(rt[i])*Nt[i]
  }
  minpop<- min(Nt)
  extinct<- ifelse(minpop< Nextinct, yes=1,no=0)
  output[j]<- extinct
}

sum(output)
##  The population dropped below the extinction threshold 29 times. (for a specific run)


### Q2 

tmax<- length(grizzly$Year)
rt_vector<- rep(x=NA, times=tmax)

for(i in 1:(tmax)){
  rt<-log((grizzly$Est_Total_Females[i+1])/grizzly$Est_Total_Females[i])
  rt_vector[i]<- rt 
}

mean(rt_vector, na.rm=TRUE)
sd(rt_vector, na.rm=TRUE)


grizzly_model<- rep(x=NA, times=1000)
for(j in 1:1000){
  
  rbar<- 0.04521859
  sd<- 0.1200323
  N0<- 139
  tmax<- 100
  Nextinct<- 75
  years<- 0:tmax
  Nt<- rep(x=NA, times=tmax)
  
  rt<- rnorm(n=length(years), mean=rbar, sd=sd)
  Nt[1]<- N0
  for(i in 1:tmax){
    Nt[i+1]<- exp(rt[i])*Nt[i]
  }
  minpop<- min(Nt)
  extinct<- ifelse(minpop< Nextinct, yes=1,no=0)
  grizzly_model[j]<- extinct
}

(sum(grizzly_model))/1000
