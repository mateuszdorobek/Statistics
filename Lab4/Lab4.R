
#Lab4
setwd("F:/MiNI IAD/Sem 1/Statystyka Matematyczna/Laboratoria/Statistics/Lab4")
auta = read.csv2("samochody.csv")
summary(auta)

#------
auta$zp = (1/auta$mpg)*(378.5/1.609)
b <- boxplot(auta$zp)
hist(auta$zp, prob=TRUE, 50)
points(density(auta$zp, na.rm = TRUE), type='l', col=2)
points(density(auta$zp, na.rm = TRUE, kernel="epanechnikov"), type='l', col=3)
points(density(auta$zp, na.rm = TRUE, bw=0.4), type='l', col=4)

#------
install.packages("fBasics")
library(fBasics)
skewness(auta$zp, na.rm = TRUE)
kurtosis(auta$zp, na.rm = TRUE)

# ----
auta$kat[auta$zp <= 7] = 'm'
auta$kat[12 >= auta$zp & auta$zp > 7] = 's'
auta$kat[auta$zp > 12] = 'd'

sum(auta$kat == 'm', na.rm=TRUE)
table(auta$kat)

spalanie = cut(auta$zp, c(-Inf, 7,12,Inf),rigth = TRUE, labels = c('m','s','d'))
table(spalanie)

#-----
tapply(auta$zp, auta$prod, mean, na.rm=TRUE)
tapply(auta$zp, auta$prod, sd, na.rm=TRUE)
#tak jest brzydko
tapply(auta$zp, auta$prod, boxplot, na.rm=TRUE)
#tak jest fajno
b = boxplot(auta$zp~auta$prod)
b$out[b$group==2]

#Zad 9
b = boxplot(auta$zp~auta$cylindry)
b$out[b$group==3]
b$out[b$group==4]
b$out[b$group==5]
tapply(auta$zp, auta$cylindry, sd, na.rm=TRUE)

#Zad11
moc = auta$moc[81 >= auta$rok & auta$rok >= 79]
quantile(moc, 0.95, na.rm = TRUE)

#Zad 12
moc = auta$przysp[3000 >= auta$waga & auta$waga >= 2500]
quantile(moc, 0.75, na.rm = TRUE)

