library(readxl)
library(DescTools)
library(tidyverse)

p11 <- readxl::read_excel("C:\Users\eajtrnu\Desktop\OSS\4.1\Statistika\Seminar\p13.xlsx", #edit the data file path, according to your need
                  range = "A2:A311")
vrijeme = p11$vrijeme

#1.
min(vrijeme) # minimalni broj od danih
max(vrijeme) # maksimalni broj od danih 
mean(vrijeme) # aritmetička sredina
var(vrijeme) # varijacija
sd(vrijeme) # devijacija
median(vrijeme) #srednji
quantile(vrijeme) # interkvartalni razvoj
summary(vrijeme) # karakteristična petorka
mode(vrijeme) # vrsta podataka
#dodat jos box model ali pogledat modle

#2.


breaks=seq(310,4650, by=868)
breaks

time.cut=cut(p11$vrijeme,breaks,right=FALSE)
time.freq=table(time.cut)
time.freq

transform(table(time.freq))
relativna_frekvencija = transform(time.freq,Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
relativna_frekvencija
x
x=factor(vrijeme)
bins=levels(x)
bins
count=cut(vrijeme,bins)
FreqTable=transform(table(count))
cumuTable=cumsum(table(count))
transform(table(cumuTable))
View(transform(FreqTable,cumFreq=cumsum(Freq),relative=prop.table(Freq)))


#3. 
qplot(x=vrijeme,geom = "histogram", xlab = "Vrijeme čekanja na policiji(minute)", ylab = "Frekvencija",
      main = "Histogram frekvencija", col=I("darkblue"),fill=I("lightblue"),bins = 10)
qplot(x=vrijeme,geom = "histogram", xlab = "Vrijeme čekanja na policiji(minute)", ylab = "Relativne Frekvencija",
      main = "Histogram frekvencija", col=I("darkblue"),fill=I("lightblue"),bins = 10)
qplot(x=vrijeme,geom = "freqpoly", xlab = "Vrijeme čekanja na policiji(minute)", ylab = "Frekvencija",
      main = "Histogram frekvencija", col=I("darkblue"),bins = 10)
#4.
df=length(vrijeme)-1
sdSqrt = sd(vrijeme)/sqrt(length(vrijeme))

#90%
print(error <- qt(0.90,df)*sdSqrt)

print(left <- mean(vrijeme)-error)
print(right <- mean(vrijeme)+error)
#95%
print(error <- qt(0.95,df)*sdSqrt)

print(left <- mean(vrijeme)-error)
print(right <- mean(vrijeme)+error)
#99% 
print(error <- qt(0.99,df)*sdSqrt)

print(left <- mean(vrijeme)-error)
print(right <- mean(vrijeme)+error)

#5
t.test(vrijeme,mu=mean(vrijeme),conf.int=0.95)
 
