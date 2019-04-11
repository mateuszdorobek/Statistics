#1------------
# curve(dnorm(x,m,s),from=-6,to=6)
#a
curve(dnorm(x,0,1), xlim=c(-5,5))
curve(dnorm(x,1,1), xlim=c(-5,5),add=TRUE, col=2)
curve(dnorm(x,2,1), xlim=c(-5,5),add=TRUE, col=3)
#b
curve(dnorm(x,0,1), xlim=c(-5,5), ylim=c(0,0.8))
curve(dnorm(x,0,0.5), xlim=c(-5,5),add=TRUE, col=2)
curve(dnorm(x,0,2), xlim=c(-5,5),add=TRUE, col=2)

#2-------------

#Do Domu - dla innego rozkładu niż normalny oraz dla couchyego -3, 3
curve(dnorm(x,0,1), xlim=c(-5,5))

#3------------
#a
pnorm(179,173,6)
#b
pnorm(180,173,6)-pnorm(167,173,6)
#c
1-pnorm(181,173,6)
pnorm(181,173,6, lower.tail=FALSE)
#d
qnorm(0.6,173,6)

#4-------------
#a 
qnorm(0.95)
#b
qnorm(0.975)
#c
qt(0.95,10)
#d
qt(0.99,20)
#e
qchisq(0.9,4)
#f
qchisq(0.95,10)
#g
qf(0.95,2,10)
#h
qf(0.99,3,18)

#5------------
par(mfrow=c(1,2))
#mfcol - wypełnia kolumnami
#a
curve(dgamma(x,1,1), xlim=c(0,10), ylim=c(0,1.7))
curve(dgamma(x,0.5,1), add=TRUE, col=2)
curve(dgamma(x,2,1), add=TRUE, col=3)
curve(dgamma(x,3,1), add=TRUE, col=4)
#b
curve(dgamma(x,2,1), xlim=c(0,10), ylim=c(0,1.7))
curve(dgamma(x,2,2), add=TRUE, col=2)
curve(dgamma(x,2,3), add=TRUE, col=3)

#6-----------------
par(mfrow=c(1,2))
#a
curve(dchisq(x,5), xlim=c(0,100), ylim=c(0,0.2))
curve(dchisq(x,10), add=TRUE, col=2)
curve(dchisq(x,40), add=TRUE, col=3)
#b
m=40
curve(dnorm(x,m,sqrt(2*m)), xlim=c(0,2*m), ylim=c(0,0.05))
curve(dchisq(x,m), add=TRUE, col=3)

#7-----------------
par(mfrow=c(1,2))

curve(dt(x,1), xlim=c(-10,10), ylim=c(0,0.4))
curve(dt(x,5), add=TRUE, col=2)
curve(dt(x,30), add=TRUE, col=3)

curve(dt(x,30), xlim=c(-10,10), ylim=c(0,0.4), col=3)
curve(dnorm(x,0,1), add=TRUE, col=4)

#8---------------
par(mfrow=c(1,3))
#a
curve(df(x,10,5), xlim=c(0,10), ylim=c(0,0.9))
curve(df(x,10,10), add=TRUE, col=2)
curve(df(x,10,20), add=TRUE, col=3)
#b
curve(df(x,5,2), xlim=c(0,10), ylim=c(0,0.9))
curve(df(x,3,2), add=TRUE, col=2)
curve(df(x,2,2), add=TRUE, col=3)
#c
curve(df(x,2,1), xlim=c(0,10), ylim=c(0,0.9))
curve(df(x,2,5), add=TRUE, col=2)
curve(df(x,2,10), add=TRUE, col=3)
curve(df(x,2,20), add=TRUE, col=4)




#5-Cudowne-Chwile-----------
#Do domu

