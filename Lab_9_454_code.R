# Bayesian Lab

#### Q1 

# Calculating likelihood of each p value
tosses<- 10
heads <- 8
p<- seq(0.01 , 0.99 , 0.01)
lik_vec<- rep(NA, times=length(p))

for(i in 1:length(p)){
  lik <- dbinom(heads, size = tosses, prob = p[i], log = FALSE)
  lik_vec[i]<- lik
}
 
# Calculating prior of each p value (using uniform distribution)
prior<- rep(1/length(p), times=length(p)) #  0.01

# Calculating prior belief that 0.45 ≤ p ≤0.55  
prior_belief <- length(which(p >= 0.45 & p <= 0.55 )) * prior # 0.11

# Calculating posterior prob numerator 
num<- lik_vec* prior

# denominator
denom<- sum(num) # 0.09182733

# New probabilities for each p 
new_probs<- num/denom

# Posterior probability that 0.45 ≤ p ≤0.55  
posterior <- sum(new_probs[p >= 0.45 & p <= 0.55 ]) # 0.056



#### Q2 (repeat of above with beta distribution)

tosses<- 10
heads <- 8
p<- seq(0.01 , 0.99 , 0.01)
lik_vec<- rep(NA, times=length(p))

for(i in 1:length(p)){
  lik <- dbinom(heads, size = tosses, prob = p[i], log = FALSE)
  lik_vec[i]<- lik
}

prior_vec <- rep(NA, length(p))
  for(i in 1:length(p)){
     prior_vec[i] <- dbeta(p[i], 20, 20, log = FALSE)
  }

prior_belief <-  sum(prior_vec[which(p >= 0.45 & p <= 0.55 )]) # 51.22704/100 = 0.51

num <- lik_vec * prior_vec

denom<- sum(num) 

new_probs<- num/denom

posterior <- sum(new_probs[p >= 0.45 & p <= 0.55 ]) # 0.4160918

