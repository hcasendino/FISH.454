### HW 4: Stochastic Models

#========================== 
# Testing the model with Est_Total_Females

# Getting the sample mean & sd 
tmax<- length(grizzly$Year)
rt_vector<- rep(x=NA, times=tmax)
for(i in 1:(tmax)){
  rt<-log((grizzly$Est_Total_Females[i+1])/grizzly$Est_Total_Females[i])
  rt_vector[i]<- rt 
}
r.bar<- mean(rt_vector, na.rm=TRUE)
r.se<- (sd(rt_vector, na.rm=TRUE))/(sqrt(27))
r.sd<- sd(rt_vector, na.rm=TRUE)
## Model with variation in sample mean and sampled annual growth rate 
grizzly_model<- rep(x=NA, times=1000)
for(j in 1:1000){
  
  mu <- rnorm(1, mean= r.bar, sd = r.se)
  rt<- rnorm(n=length(years), mean=mu, sd=r.sd)
  N0<- 139
  tmax<- 100
  Nextinct<- 75
  years<- 0:tmax
  Nt<- rep(x=NA, times=tmax)
 
   Nt[1]<- N0
  for(i in 1:tmax){
    Nt[i+1]<- exp(rt[i])*Nt[i]
  }
  minpop<- min(Nt)
  extinct<- ifelse(minpop< Nextinct, yes=1,no=0)
  grizzly_model[j]<- extinct
}
(sum(grizzly_model))/1000

#====================
### Testing the model with Observed_Females_cubs 

tmax<- length(grizzly$Year)
rt_vector<- rep(x=NA, times=tmax)
for(i in 1:(tmax)){
  rt<-log((grizzly$Observed_Females_cubs[i+1])/grizzly$Observed_Females_cubs[i])
  rt_vector[i]<- rt 
}
r.bar<- mean(rt_vector, na.rm=TRUE)
r.se<- (sd(rt_vector, na.rm=TRUE))/(sqrt(27))
r.sd<- sd(rt_vector, na.rm=TRUE)
## Model with variation in sample mean and sampled annual growth rate 
grizzly_model<- rep(x=NA, times=1000)
for(j in 1:1000){
  
  mu <- rnorm(1, mean= r.bar, sd = r.se)
  rt<- rnorm(n=length(years), mean=mu, sd=r.sd)
  N0<- 51
  tmax<- 100
  Nextinct<- 75
  years<- 0:tmax
  Nt<- rep(x=NA, times=tmax)
  
  Nt[1]<- N0
  for(i in 1:tmax){
    Nt[i+1]<- exp(rt[i])*Nt[i]
  }
  minpop<- min(Nt)
  extinct<- ifelse(minpop< Nextinct, yes=1,no=0)
  grizzly_model[j]<- extinct
}
(sum(grizzly_model))/1000


#====================
### Testing the model with Est_Females_cubs 

tmax<- length(grizzly$Year)
rt_vector<- rep(x=NA, times=tmax)
for(i in 1:(tmax)){
  rt<-log((grizzly$Est_Fem_w_cub[i+1])/grizzly$Est_Fem_w_cub[i])
  rt_vector[i]<- rt 
}
r.bar<- mean(rt_vector, na.rm=TRUE)
r.se<- (sd(rt_vector, na.rm=TRUE))/(sqrt(27))
r.sd<- sd(rt_vector, na.rm=TRUE)
## Model with variation in sample mean and sampled annual growth rate 
grizzly_model<- rep(x=NA, times=1000)
for(j in 1:1000){
  
  mu <- rnorm(1, mean= r.bar, sd = r.se)
  rt<- rnorm(n=length(years), mean=mu, sd=r.sd)
  N0<- 56
  tmax<- 100
  Nextinct<- 75
  years<- 0:tmax
  Nt<- rep(x=NA, times=tmax)
  
  Nt[1]<- N0
  for(i in 1:tmax){
    Nt[i+1]<- exp(rt[i])*Nt[i]
  }
  minpop<- min(Nt)
  extinct<- ifelse(minpop< Nextinct, yes=1,no=0)
  grizzly_model[j]<- extinct
}
(sum(grizzly_model))/1000
