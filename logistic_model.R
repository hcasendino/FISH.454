# install manipulate package if needed)
if (!require("manipulate", character.only = TRUE)) {
  install.packages("manipulate", dependencies = TRUE)
}
library("manipulate")


# Logistic Model Demo



# Simulate trajectory
run.logistic<-function(r,K){
s <- 1 - r # slope of the recusive function
N0 <- 0.3
tmax <- 50
years <- 0:tmax
output <- rep(NA, length(years))
output[1] <- N0
for (i in 1:tmax) {
  n.t <- output[i]
  n.t.plus.1 <- n.t + n.t * r * (1 - n.t/K)
  output[i+1] <- n.t.plus.1
}
# Make time dynamic plot
par(mfrow=c(2,2), mar = c(5,5,1,1), las =1)
plot(
  x = years,
  y = output,
  type = 'l',
  xlim = c(0, 50),
  ylim = c(0, 1.5),
  col = "black",
  xlab = 'Time',
  ylab = 'Abundance',
  main = paste0("slope = ", round(s, 4))
)
plot(
  x = years,
  y = output,
  type = 'l',
  xlim = c(0, 50),
  ylim = c(K - 0.005, K + .005),
  col = "black",
  xlab = 'Time',
  ylab = 'Abundance',
  main = "zoom in near K"
)



# set up cobwebbing
nt <- seq( from = 0, to = 2.0 * K, length.out = 100)
ntplus1 <- nt + nt * r * (1 - nt / K)

plot(x = nt, y = ntplus1,
     type = "l",
     lwd = 3,
     col = "blue",
     xlab = "Nt",
     ylab = "N t+1",
     ylim = c(0, 1.5 * K),
     xlim <- c(0, 1.5 * K)
)
abline(a = 0, b = 1,
       col = "red",
       lwd = 3)

# plot vertical lines
for (i in 1:tmax){
  xs <- rep(x = output[i], times = 2)
  ys <- c(output[i], output[i+1])
  lines(xs, ys,
        lwd = 2
  )
}
# plot horizontal lines
for (i in 1:tmax) {
  xs <- c(output[i], output[i+1])
  ys <- rep (x = output[i+1], times = 2)
  lines(xs, ys,
        lwd = 2
  )
}

# Repeat, but remove transient dynamics
plot(x = nt, y = ntplus1,
     type = "l",
     lwd = 3,
     col = "blue",
     xlab = "Nt",
     ylab = "N t+1",
     ylim = c(0, 1.5 * K),
     xlim <- c(0, 1.5 * K)
)
abline(a = 0, b = 1,
       col = "red",
       lwd = 3)

# plot vertical lines
for (i in 30:tmax){
  xs <- rep(x = output[i], times = 2)
  ys <- c(output[i], output[i+1])
  lines(xs, ys,
        lwd = 2
  )
}
# plot horizontal lines
for (i in 30:tmax) {
  xs <- c(output[i], output[i+1])
  ys <- rep (x = output[i+1], times = 2)
  lines(xs, ys,
        lwd = 2
  )
}

mtext('Transient Dynamics Removed',side=3)
par(mfrow=c(1,1))
}


manipulate(run.logistic(r,K),
           r=slider(0.25,2.75,initial=0.5),
           K=slider(0.5,2,initial=1)
)

