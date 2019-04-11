#2-------------

#Do Domu - dla innego rozkładu niż normalny oraz dla couchyego -3, 3
par(mfrow=c(1,2))

curve(dnorm(x,0,1), xlim=c(-5,5))
abline(v = -3, col="red", lwd=3, lty=2)
abline(v = 3, col="red", lwd=3, lty=2)

curve(dcauchy(x,0,1), xlim=c(-5,5))
abline(v = -3, col="red", lwd=3, lty=2)
abline(v = 3, col="red", lwd=3, lty=2)

#5-Cudowne-Chwile-----------
par(mfrow=c(1,3))

curve(dnorm(x,0,1), xlim=c(-5,5))
abline(v = -1, col="red", lwd=3, lty=2)
abline(v = 1, col="red", lwd=3, lty=2)

curve(dnorm(x,0,1), xlim=c(-5,5))
abline(v = -2, col="red", lwd=3, lty=2)
abline(v = 2, col="red", lwd=3, lty=2)

curve(dnorm(x,0,1), xlim=c(-5,5))
abline(v = -3, col="red", lwd=3, lty=2)
abline(v = 3, col="red", lwd=3, lty=2)

par(mfrow=c(1,1))

s = 1
m = 0
n = 10000
x <- seq(-4, 4, length=n)*s+m
hist(hx, xlab="x value",
     ylab="Density", main="Normal Standard Distributions on 10k generated elements")
hx <- rnorm(x,m,s)

#obliczona
sum(hx<1 & hx>-1)/n
#prawdziwa
pnorm(1)-pnorm(-1)

#obliczona
sum(hx<2 & hx>-2)/n
#prawdziwa
pnorm(2)-pnorm(-2)

#obliczona
sum(hx<3 & hx>-3)/n
#prawdziwa
pnorm(3)-pnorm(-3)
