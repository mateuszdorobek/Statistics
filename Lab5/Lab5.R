# Zad1

n = 20
mi = 0
sig = 1
x = rnorm(n, mi, sig)
plot(ecdf(x))
curve(pnorm(x, mi, sig), add = T, col = 2)

n = 100
x = rnorm(n, mi, sig)
plot(ecdf(x))
curve(pnorm(x, mi, sig), add = T, col = 2)

# Zad2
N = 500
a = 0
b = 1
x = rcauchy(N, a, b)
s = cumsum(x)
n = 1:N
Mn = s / n
plot(n, Mn, type = 'l', col = 1)
abline(h = 0)
Med = c()
for (i in 1:N) {
  Med[i] = median(x[1:i])
}
plot(n,
     Med,
     type = 'l',
     col = 2,
     add = TRUE)
abline(h = 0)

# czêœæ druga
N = 500
x = rcauchy(N, a, b)
s = c()
r = c()
for (i in 2:N) {
  s[i - 1] = sd(x[1:i])
  r[i - 1] = IQR(x[1:i]) / 2
}
plot(
  2:N,
  s,
  type = 'l',
  log = 'y',
  las = 1,
  ylim = c(0.05, 500)
)
lines(2:N, r, lty = 1, col = 2)
abline(h = 1)

#Zad 3
N = 500
a = 0
b = 1
x = rnorm(N, a, b)
s = cumsum(x)
n = 1:N
Mn = s / n
plot(n, Mn, type = 'l', col = 1)
abline(h = 0)
Med = c()
for (i in 1:N) {
  Med[i] = median(x[1:i])
}
lines(n,
      Med,
      type = 'l',
      col = 2,
      add = TRUE)
abline(h = 0)

# czêœæ druga
N = 5000
x = rnorm(N, a, b)
s = c()
r = c()
for (i in 2:N) {
  s[i - 1] = sd(x[1:i])
  r[i - 1] = IQR(x[1:i]) / 1.35
}
plot(2:N,
     s,
     type = 'l',
     log = 'y',
     las = 1)
lines(2:N, r, lty = 1, col = 2)
abline(h = 1)

# Zad4
M = 10000
n = 200
theta = 17
wyniki = replicate(M, {
  x = runif(n, 0, theta)
  c(2*mean(x),max(x),((n+1)/n)*max(x))
})

dim(wyniki)
wyniki[,1:5]

#obciazenie
(obc1 = mean(wyniki[1,])-theta)
(obc2 = mean(wyniki[2,])-theta)
(obc3 = mean(wyniki[3,])-theta)

#Mean Square Error
var(wyniki[1,]+obc1^2)
var(wyniki[2,]+obc2^2)
var(wyniki[3,]+obc3^2)

par(mfrow=c(1,3))
boxplot(wyniki[1,])
title("Mean")
boxplot(wyniki[2,])
title("Max")
boxplot(wyniki[3,])
title("(n+1)/n * Max")
