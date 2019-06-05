# Wejœæiówka 

#Zad 1  
  library(MASS)
  library(lattice)
  yield = barley$yield
  site = barley$site
  data = yield[site=="Waseca"]
  library(TeachingDemos)
  sqrt(sigma.test(data, conf.level = 0.99)$conf)
  
#Zad 2  
  x_0 = rep(0, 207)
  x_1 = rep(1, 33)
  x = c(x_0, x_1)

  binom.test(x = 33, n = 240, conf.level = 0.92)$conf

# Laboratorium

#Zadanie 2
  x = c(142, 151, 148, 151, 145, 150, 141)
  t.test(x,mu = 150, alt = "two.sided")
  2*pt(-1.9704,6)    
  
#Zad 15
  x = Orange$circumference
  alfa=0.1
  t.test(x,mu=150,alt="less")
#Zad 17
  library(MASS)
  crabs
  alfa=0.04
  x = crabs$CW[crabs$sp=='B']
  y = crabs$CW[crabs$sp=='O']
  test = t.test(x,y,alt="less",var.equal=T)
  test$p.value > alfa
  