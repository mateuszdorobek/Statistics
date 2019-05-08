#Zestaw 1
#Zad 10
n = 100
p = 0.5
k = 0:n
plot(k, dbinom(k, n, p), type = "h")
mi = n * p
sigma = sqrt(n * p * (1 - p))
curve(dnorm(x, mi, sigma), add = TRUE, col = 3)
lambda = n * p
points(k, dpois(k, lambda), col = 2, pch = 16)

#Zad 11
#R dzia³a tak, ¿e w rozk³dzie geometrycznym patrzy na Y
p = 0.1
q = 1 - p
vec = 0:3
dgeom(vec, p)
#p-stwo ¿e x>11, czyli p-stwo ¿e y>10 czyli 1 - dyst(10)
k = 11
1 - pgeom(k - 1, p)

#Zad 12
k = 0 #liczba z³ych w tych wyci¹giêtych 
N = 200 # licznoœæ próbki
n = 10 # ile wyci¹gasz
m = 5 #liczba z³ych ogó³em
dhyper(k, m, N - m, n)

#Zad 13
#a)
lambda = 1.0e-4
vec = c(1000, 10000, 30000)
1 - pexp(vec, lambda)
#b)
qexp(0.1, lambda)
qexp(0.5, lambda)

#Zad 14
x = 4 #œrednia liczba klientów na godzinê
#b)
lambda = x #EX = lambda
lambda_hat = labmda #
std = 1 / lambda
#c)???
pexp(0.5, 1 / lambda)
#d)??? X=0 Y>1 x poiss y exp
1 - pexp(1)
#Zad 15
n = 1.0e4
y = runif(n, 0, 1)
mean(y ^ 2)

#monte carlo
#a)
n = 10000
x = runif(n, 0, 1)
y = runif(n, 0, 1)
plot(
  x,
  y,
  xlim = c(0, 1),
  ylim = c(0, 1),
  las = 1,
  pch = '.'
)
curve(x ^ 2, add = TRUE, col = 2, lwd = 2)
z = (y < x ^ 2)
sum(z == TRUE) / length(z)
points(x[z], y[z], col = 3, pch = "*")

quantile(x, 0.5)
range(x)

#Zestaw 2
