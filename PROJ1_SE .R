# Southeast 
E_t_vect<- c(138,118,82)
tmax<- 25
q<- 7.880401e-07


#==================================
# R & K variance 
# K_list lists the low, average and high estimates of K for each population, using estimates of pre-fishing population sizes (when dolphins were near equilibrium). 

N_0_vect<- c(140,1811,1230)
r_vect<- c(0.025,0.037,0.049)
output<- data.frame("Low r" = rep(NA, length=3), "Mean r"= rep(NA, length=3), "High r"= rep(NA, length=3))
row.names(output) <- c("low K","mean K","high K")

for(l in 1:length(r_vect)){
  r<- r_vect[l]
  
  K_list<- c(4340, 4438, 5045)
  final_N<- rep(NA, length=3)
  
  for(i in 1:length(K_list)) {
    K<- K_list[i]
    N_t<- N_0_vect[2]
    d_kills<- q*E_t_vect[2]*365
    
    for(j in 1:tmax){
      N_t<- N_t*(1 + ((r* (1-(N_t/K))) - d_kills))
    }
    final_N[i]<- N_t
  }
  # get percent change of North population with various population sizes 
  output[,l]<- ((final_N - N_0_vect[2])/N_0_vect[2])*100
}
output



#===================================
#N0 & K variance
r<- mean(c(0.025, 0.049))

# pop specific 
N_0_list<- c(1399,1811,2344)
d_kills<- q*E_t_vect[2]*365
K_list<- c(4340, 4438, 5045)
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
N_0_list<- c(1399,1811,2344)
d_kills<- q*E_t_vect[2]*365
output<- data.frame("Low N0" = rep(NA, length=3), "Mean N0"= rep(NA, length=3), "High N0"= rep(NA, length=3))
row.names(output) <- c("low r","mean r","high r")

for(p in 1:length(r_vect)){
  r<- r_vect[p]
  final_N<- rep(NA, length=3)
  K<- K_vect[2]
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
low K  -35.51158 -23.16281 -10.180612
mean K -35.23374 -22.66519  -9.405277
high K -33.71364 -19.90519  -5.045512

# #N0 & K 
output
          Low.N0   Mean.N0   High.N0
low K  -17.71974 -23.16281 -29.24912
mean K -17.28151 -22.66519 -28.69931
high K -14.86844 -19.90519 -25.62473

# N0 and r 
output
          Low.N0    Mean.N0   High.N0
low r  -32.3004957 -35.233745 -38.68329
mean r -17.2815071 -22.665187 -28.69931
high r  -0.8054982  -9.405277 -18.59292


