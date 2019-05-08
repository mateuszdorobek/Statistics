# p = rnorm(10000)
# hist(p,100, add=TRUE)
# x = 
# curve(dnorm(x,0,1)*500,xlim=c(-2,2))

# par(mfrow=c(1,2))
# par(mfrow=c(1,1))


# Wejściówka
#X - rozkład t-studenta z 12 stopniami swobody 
x = rt(1000,df=12)
hist(x,200)
abline(v=1.8)
abline(v=-0.55)
# kwantyl rzędu 0.74
qt(0.74,12)
# P(-0.55<X<1.8)
pt(1.8,12)-pt(-0.55,12)

#Zestaw 2
#Zad 1
dane = c(17364000,56128000,11239000,8170000)
etykiety = c("Panny","Mężatki","Wdowy","Rozwódki")
proc <- round(prop.table(dane)*100,1)
par(mfrow=c(1,2))
pie(x=dane,labels=paste(etykiety,paste(proc,"%",sep=""),sep="\n"),main='PiePlot')
barplot(height=dane,names.arg=etykiety,col = rainbow(4),ylim=c(0,1.2*max(dane)),main='BarPlot')

#Zad 2

stacje = read.csv2("stacje.csv")
x=table(stacje)
proc <- round(prop.table(x)*100,1)
par(mfrow=c(1,2))
pie(table(stacje),main='PiePlot')
plot(stacje,col = rainbow(4),ylim=c(0,400),main='BarPlot')

#Zad 3
x=scan(nlines=2)
plot(x,type="b")

#Zad 4

#a)
#b)
butelki = read.csv2("butelki.csv")
butelki$cisn <- butelki$strength*0.0068947
par(mfrow=c(1,1))
h <- hist(x=butelki$cisn,breaks=13)
length(h$mids)==13
hist(x=butelki$cisn,100)
#c)

#d)
stem(x=butelki$cisn,2)


