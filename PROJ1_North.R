#### North Population
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

K_list<- c(437,448,524)
final_N<- rep(NA, length=3)

for(i in 1:length(K_list)) {
  K<- K_list[i]
  N_t<- N_0_vect[1]
  d_kills<- q*E_t_vect[1]*365
  
  for(j in 1:tmax){
    N_t<- N_t*(1 + ((r* (1-(N_t/K))) - d_kills))
  }
  final_N[i]<- N_t
}
# get percent change of North population with various population sizes 
output[,l]<- ((final_N - N_0_vect[1])/N_0_vect[1])*100
}
output




#===================================
# N0 & K variance 
r<- mean(c(0.025, 0.049))

# pop specific 
N_0_list<- c(46,140,280)
d_kills<- q*E_t_vect[1]*365
K_list<- c(437,448,524)
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
N_0_list<- c(46,140,280)
d_kills<- q*E_t_vect[1]*365
output<- data.frame("Low N0" = rep(NA, length=3), "Mean N0"= rep(NA, length=3), "High N0"= rep(NA, length=3))
row.names(output) <- c("low r","mean r","high r")

for(p in 1:length(r_vect)){
  r<- r_vect[p]
  final_N<- rep(NA, length=3)
  K<- K_vect[1]
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








#============
# NORTH 

#R K 
output
Low.r    Mean.r     High.r
low K  -41.07861 -27.57404 -12.575310
mean K -40.86402 -27.16769 -11.904396
high K -39.59771 -24.73798  -7.836277

# N0 K
output
      Low.N0   Mean.N0   High.N0
low K  -14.62172 -27.57404 -41.06765
mean K -14.43904 -27.16769 -40.51993
high K -13.37163 -24.73798 -37.16199

#R N0
output
        Low.N0   Mean.N0   High.N0
low r  -34.52941 -40.86402 -48.37176
mean r -14.43904 -27.16769 -40.51993
high r  10.54744 -11.90440 -32.62600