# Zestaw 3
# Zad 5
n = 10
N = 10000
mi = 0
sigma = 2
alfa = 0.05
q = qt(1-alfa/2, n-1)

ile_wpada = replicate(N,{
  x = rnorm(n, mi, sigma)
  m = mean(x)
  s = sd(x)
  (m-q*s/sqrt(n) < mi) & (m+q*s/sqrt(n) > mi)
})
length(ile_wpada)
mean(ile_wpada)
