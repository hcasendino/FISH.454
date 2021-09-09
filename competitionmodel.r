
# Parameter Inputs
Kx<-1
Ky<-1
rx<-0.25
ry<-0.25

alpha<-1.5
beta<- 1.5


# Starting Values
Xstart<-0.8
Ystart<-0.7



plot_iso <- function(rx, ry, Kx, Ky, alpha, beta, Xstart, Ystart) {
# Solve for equilibrium, dismiss negative values and repace with 0
Xstar<-min(Kx,max(0,(alpha*Ky - Kx)/(alpha*beta - 1)))
Ystar<-min(Ky,max(0,(beta*Kx - Ky)/(alpha*beta -1)))

# Plot isoclines
xmax <- Kx*1.1
ymax <- Ky*1.1

x.2.use <- c(0, xmax)

x.isocline <- (Kx - x.2.use)/alpha
y.isocline <- Ky - x.2.use * beta

# make final plot
par(xpd = F)
plot(
  x.2.use,
  x.isocline,
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "X",
  ylab = "Y",
  xlim = c(0, xmax),
  ylim = c(0, ymax),
  las = 1,
  xaxs = "i",
  yaxs = "i"
)
lines(x.2.use, y.isocline,
      lwd = 2,
      col = "red")

# add in vertical and horizontal isoclines:
abline(h = 0, col = "red", lwd =3)
abline(v = 0, col = "blue", lwd =3)

# Plot all possible equilibria
par(xpd = T)
points(x=0, y = 0 , pch = 21, bg = "white", cex = 1.5)
points(x = 0, y = Ky, pch = 21, bg = "white", cex = 1.5)
points(x = Kx,y = 0, pch = 21, bg = "white", cex = 1.5)
points(x = Xstar, y = Ystar, pch = 21, bg = "white", cex = 1.5)
par(xpd = F)

legend("topright",legend=c("X isocline", "Y Isocline"),lwd=2, col=c("blue","red"))

points(Xstart,Ystart,pch=21,bg="black",cex=1.5)
# Run model trajectore

readline(prompt="Press [enter] to see trajectory")
timelist<-seq(1,150)

N.output<-matrix(NA,nrow=150,ncol=3)

N.output[1,1:3]<-c(1,Xstart,Ystart)

for (i in 2:150){
  X<-N.output[i-1,2]
  Y<-N.output[i-1,3]
  N.output[i,2]<-X+rx*X*(1-(X+alpha*Y)/Kx)
  N.output[i,3]<-Y+ry*Y*(1-(Y+beta*X)/Ky)
}

lines(N.output[,2],N.output[,3],col="black",lwd=2)


# Calculate eigenvalues
Jacob=matrix(0,2,2)
Jacob[1,1]<-1+rx-2*rx*Xstar/Kx-rx*alpha*Ystar/Kx
Jacob[1,2]<--alpha*rx*Xstar/Kx
Jacob[2,1]<--beta*ry*Ystar/Ky
Jacob[2,2]<-1+ry-2*ry*Ystar/Ky-ry*beta*Xstar/Ky


ev<-eigen(Jacob)

Eigen.Magnitude=(Re(ev$values[1])^2+Im(ev$values[1])^2)^0.5
print(paste("Eigenvalue magnitude = ", round(Eigen.Magnitude, 2)))
      
}







plot_iso(rx, ry, Kx, Ky, alpha, beta, Xstart, Ystart)


