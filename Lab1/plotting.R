s = 1
m = 0
n=10000
p=2
x <- seq(-4, 4, length=n)*s+m
hist(hx, xlab="x value",
     ylab="Density", main="Normal Standard Distributions")
hx <- rnorm(x,m,s)
# plot(x, hx, type="l", lty=2, xlab="x value",
#      ylab="Density", main="Comparison of t Distributions")


sum(hx<1 & hx>-1)/n
pnorm(1)-pnorm(-1)

sum(hx<2 & hx>-2)/n
pnorm(2)-pnorm(-2)

sum(hx<3 & hx>-3)/n
pnorm(3)-pnorm(-3)

pnorm(x) #dystyrybuanta
dnorm(x,m,s) #gęstość
qnorm(p,m,s) #kwantyl rzędu p
hist(rnorm(n,m,s),100)#generowanie

pnorm(179,173,6)
pnorm(180,173,6)-pnorm(167,173,6)

# degf <- c(1, 3, 8, 30)
# colors <- c("red", "blue", "darkgreen", "gold", "black")
# labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

# for (i in 1:4){
#   lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
# }
# 
# legend("topright", inset=.05, title="Distributions",
#        labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
