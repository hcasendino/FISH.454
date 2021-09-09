#### SW  Population
E_t_vect<- c(138,118,82)
tmax<- 25
q<- 7.880401e-07

#===================================
# R & K variance 
# K_list lists the low, average and high estimates of K for each population, using estimates of pre-fishing population sizes (when dolphins were near equilibrium). 

N_0_vect<- c(140,1811,1230)
r_vect<- c(0.025,0.037,0.049)
output<- data.frame("Low r" = rep(NA, length=3), "Mean r"= rep(NA, length=3), "High r"= rep(NA, length=3))
row.names(output) <- c("low K","mean K","high K")

for(l in 1:length(r_vect)){
  r<- r_vect[l]
  
  K_list<- c(2159,2191, 2389)
  final_N<- rep(NA, length=3)
  
  for(i in 1:length(K_list)) {
    K<- K_list[i]
    N_t<- N_0_vect[3]
    d_kills<- q*E_t_vect[3]*365
    
    for(j in 1:tmax){
      N_t<- N_t*(1 + ((r* (1-(N_t/K))) - d_kills))
    }
    final_N[i]<- N_t
  }
  # get percent change of North population with various population sizes 
  output[,l]<- ((final_N - N_0_vect[3])/N_0_vect[3])*100
}
output



#===================================
#N0 & K variance
r<- mean(c(0.025, 0.049))

# pop specific 
N_0_list<- c(1032,1230,1665)
d_kills<- q*E_t_vect[3]*365
K_list<- c(2159,2191, 2389)
output<- data.frame("Low N0" = rep(NA, length=3), "Mean N0"= rep(NA, length=3), "High N0"= rep(NA, length=3))
row.names(output) <- c("low K","mean K","high K")

for(p in 1:length(K_list)){
  K<- K_list[p]
  final_N<- rep(NA, length=3)
  
  for(i in 1:length(N_0_list)){
    N_t<- N_0_list[i]
    
    for(j in 1:tmax){
      N_t<- N_t*(1 + ((r* (1-(N_t/K))) - d_kills))
    }
    final_N[i]<- N_t
  }
  output[p,]<- ((final_N - N_0_list)/N_0_list)*100
}
output







#==========================
# N0 and r variance 

K_vect<- c(448,4438,2191)
r_vect<- c(0.025,0.037,0.049)

# pop specific 
N_0_list<- c(1032,1230,1665)
d_kills<- q*E_t_vect[3]*365
output<- data.frame("Low N0" = rep(NA, length=3), "Mean N0"= rep(NA, length=3), "High N0"= rep(NA, length=3))
row.names(output) <- c("low r","mean r","high r")

for(p in 1:length(r_vect)){
  r<- r_vect[p]
  final_N<- rep(NA, length=3)
  K<- K_vect[3]
  for(i in 1:length(N_0_list)){
    N_t<- N_0_list[i]
    
    for(j in 1:tmax){
      N_t<- N_t*(1 + ((r* (1-(N_t/K))) - d_kills))
    }
    final_N[i]<- N_t
  }
  output[p,]<- ((final_N - N_0_list)/N_0_list)*100
}
output




#===================
# R K 
output
      Low.r    Mean.r     High.r
low K  -24.17264 -14.13227 -4.4688896
mean K -23.87143 -13.63982 -3.7672618
high K -22.14361 -10.78215  0.3481955

# N0 & K
output
          Low.N0   Mean.N0   High.N0
low K  -8.384993 -14.13227 -24.58948
mean K -7.916882 -13.63982 -24.07067
high K -5.211625 -10.78215 -21.03706

#N0 and r
output
          Low.N0    Mean.N0   High.N0
low r  -20.444882 -23.871429 -30.48074
mean r  -7.916882 -13.639822 -24.07067
high r   4.551214  -3.767262 -18.17055

