# Zestaw 3
# Zad 5
n = 10
N = 10000
mi = 0
sigma = 2
alfa = 0.05
q = qt(1 - alfa / 2, n - 1)

ile_wpada = replicate(N, {
  x = rnorm(n, mi, sigma)
  m = mean(x)
  s = sd(x)
  (m - q * s / sqrt(n) < mi) & (m + q * s / sqrt(n) > mi)
})
length(ile_wpada)
mean(ile_wpada)

# Zad 10

class(iris)
# a)
x = iris[iris$Species == "virginica", "Petal.Length"]
t.test(x, conf.level = 0.99)$conf
# b)
library(TeachingDemos)
sqrt(sigma.test(x, conf.level = 0.95)$conf)
# Dla du¿ych prób
# prop.test(k, n, conf.level, corr = TRUE)
# corr - poprawka na ci¹g³oœæ
# Dla ma³ych prób procedura Klopera-Pearsona
# binom.test

# Zad 9
binom.test(x = 3, n = 12, conf.level = 0.95)$conf

# Zad 8
p = binom.test(x = 578, n = 1014, conf.level = 0.95)$conf
p[2] - p[1]
p = prop.test(
  x = 578,
  n = 1014,
  conf.level = 0.95,
  corr = TRUE
)$conf
p[2] - p[1]
p = prop.test(
  x = 578,
  n = 1014,
  conf.level = 0.95,
  corr = FALSE
)$conf
p[2] - p[1]
578 / 1014

#w³asna implementacja
prz_ufn_war = function(x, ufnosc) {
  alfa = 1 - ufnosc
  n = length(x)
  s_2 = var(x)
  q_1 = qchisq((1 - alfa / 2), n - 1)
  q_2 = qchisq(alfa / 2, n - 1)
  prz = (n - 1) * s_2 / c(q_1, q_2)
  list(prz_ufn_dla_wariancji = prz,
       prz_ufn_dla_odchylenia = sqrt(prz))
}
x = rnorm(100, 0, 7)
# x = iris[iris$Species == "virginica","Petal.Length"]
prz_ufn_war(x, 0.95)$prz_ufn_dla_odchylenia

#w³asna implementacja przedzia³u dla p-stwa
prz_ufn_p = function(k, n , ufnosc) {
  alfa = 1 - ufnosc
  s_2 = var(x)
  p_hat = k / n
  q = qnorm((1 - alfa / 2)) * sqrt((p_hat * (1 - p_hat) / n))
  
  prz = c(p_hat - q, p_hat + q)
  list(prz_ufn_dla_pstwa = prz)
}

prz_ufn_p(100, 1000, 0.95)


# Zad 13
x = faithful$eruptions
t.test(x, conf.level = 0.99)$conf

prz_ufn_mi = function(x, ufnosc) {
  alfa = 1 - ufnosc
  n = length(x)
  q = qnorm(1 - alfa / 2)
  d = q * sd(x) / sqrt(n)
  prz = c(mean(x) - d, mean(x) + d)
  list(prz_ufn_dla_mi = prz)
}

x = faithful$eruptions
prz_ufn_mi(x, 0.99)
