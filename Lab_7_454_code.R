
############# Q 1: calculating likelihood profile for r & k 

geodata<- read.csv("Hood_canal_geoducks.csv")

r.nb.profile <- function(k,x,t,r){
  nll<- rep(NA, times=length(x))
  for (i in 1:length(x)){
    nll[i] <- -dnbinom(x=x[i], size=k, mu=t[i]*r, log=T)
  }
 return(sum(nll))
}

rlist<- seq(0.1, 0.5, 0.01)
klist<- seq(0.05, 0.5, 0.01)

nll<- matrix(data=NA, nrow=length(rlist), ncol=length(klist))

for(i in 1:length(rlist)){
  r<- rlist[i]
  for(j in 1:length(klist)){
    k<- klist[j]
    nll[i,j]<- r.nb.profile(k, x=geodata$Number_Counted, 
                          t=geodata$Area_Surveyed, r=r)
  }
}

r.profile <- apply(X=nll, MARGIN=1, FUN=min)

min(r.profile) + 1.92
# 240.8074

# 95% Confidence Interval: r from 0.13 to 0.36


############# Q 2A: Estimating a for tadpole data 

tadpoles<- read.csv("TadpolesData.csv")

alist<- seq(0.2, 0.8, 0.01)
h<- 0.01

death_vec<- tadpoles$Killed
trials_vec <- tadpoles$InitialNumber
mat<- matrix(NA, nrow=length(alist), ncol=length(trials_vec))

for(i in 1:length(alist)){
  a<- alist[i]
  for(j in 1:length(trials_vec)){
    n_trial<- trials_vec[j]
    deaths<- death_vec[j]
    p_binom<- dbinom(deaths, prob = a/(1 + a*h*n_trial), size = n_trial, log = TRUE)
    mat[i,j]<- -p_binom
  }
}

sum_mat<- rowSums(mat)
alist[which(sum_mat==min(sum_mat))]
## MLE of a: 0.45

min(sum_mat)
# = 47.63646
47.63646 + 1.92

alist[21]
alist[32]
# 95% confidence interval between a = 0.4 and 0.51



############ Q 2B: Estimating a for tadpole data with h = 0.02 

h<- 0.02
mat<- matrix(NA, nrow=length(alist), ncol=length(trials_vec))

for(i in 1:length(alist)){
  a<- alist[i]
  for(j in 1:length(trials_vec)){
    n_trial<- trials_vec[j]
    deaths<- death_vec[j]
    p_binom <- dbinom(deaths, prob = a/(1 + a*h*n_trial), size = n_trial, log = TRUE)
    mat[i,j]<- -p_binom
  }
}

sum_mat<- rowSums(mat)
alist[which(sum_mat==min(sum_mat))]
## MLE of a:  0.57

min(sum_mat)
46.96355 + 1.92

alist[30]
alist[47]
# 95% confidence interval between a = 0.49 and 0.66


########## Q 2C: plot 


plot(tadpoles$InitialNumber, tadpoles$Killed, pch=16,
     xlab="Initial Number of Tadpoles", ylab="Tadpoles Killed")

# model 1 (h=0.01)
problist<- rep(NA, length.out=length(trials_vec))

for(i in 1:length(trials_vec)){
  n_trial<- trials_vec[i]
  problist[i] <-  0.45/(1 + 0.45*0.01*n_trial)
}
predicted_kill<- problist*tadpoles$InitialNumber

lines(tadpoles$InitialNumber, predicted_kill, type="l", col="red")

# model 2 (h=0.02)
problist<- rep(NA, length.out=length(trials_vec))

for(i in 1:length(trials_vec)){
  n_trial<- trials_vec[i]
  problist[i] <-  0.57/(1 + 0.57*0.02*n_trial)
}
predicted_kill<- problist*tadpoles$InitialNumber

lines(tadpoles$InitialNumber, predicted_kill, type="l", col="blue")

legend(x="topleft", legend=c("h = 0.01", "h = 0.02"), 
       col=c("red","blue"), lwd=3, cex=0.7)
