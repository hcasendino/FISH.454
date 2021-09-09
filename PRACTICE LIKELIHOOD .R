#### Calculating Maximum Likelihood Estimation 
# We observed 0,3,6, and 7 dandelions. 
# We want to estimate the parameter r (instantaneous probability), t is area of trial. 
# We'll use poisson distribution (# of successes in CONTINUOUS trials (area etc), mean=variance).


r<- seq(.01,0.5,0.005)
x<- c(0,3,6,7)
mat<- matrix(NA, nrow=99, ncol=4)

for(i in 1:length(r)){
  r_pois<- r[i]
  
  for(j in 1:length(x)){
    x_pois<- x[j]
    a <- dpois(x_pois, lambda = r_pois*20, log = TRUE)
    mat[i,j]<- a
  }
}


# For each r, sum the negative log likelihoods of each observations
row_sums<-rowSums(mat)*-1

# Find r with smallest negative log likelihood
r[which(row_sums==min(row_sums))]
# 0.2 

# Smallest Negative log likelihood
min(row_sums)

# Add 1.92 to get upper and lower limits of r
# Lazy way of doing it
min(row_sums) + 1.92
# look for this number in row_sums, identify closest likelihoods + index values

r[61]
r[23]

# Gives you 0.31 and 0.12 = 95% confidence interval 