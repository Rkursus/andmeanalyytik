# Andmeanalüütiku arenguprogramm
# 7. õppepäev
# Statistilised meetodid praktilises elus
#
# Tuleviku prognoosimine
# Autor: Kaur Lumiste


## Sissejuhatus aegridade analüüsi

# Tavaline statistika aegridade puhul ei aita, sest aegridade puhul eiratakse 
#  ühte põhilist eeldust statistikas - vaatluste sõtlumatus. 



## Holt-Winters'i meetod

# Proovime leida Holt-Winters’i meetodil Tallinki reisijate arvu prognoosi.
load(url("http://www.ms.ut.ee/mart/andmeteadus/reisijaid.RData"))
head(Tallink)
attach(Tallink)


# Teisendame andmed aegreaks:
y=ts(reisijaid, start=c(2012,5), frequency=12)
y
plot(y)

# Hindame Holt-Winters’i mudeli
m1=HoltWinters(y, seasonal="mult")
m1

# Kontrollime mudeli prognoosivõimet – kui hästi ta on varem suutnud tulevikku näha?
plot(m1)


# Prognoosime leitud mudeli abil kaks aastat (24 kuud) tulevikku
install.packages('forecast')
library(forecast)

prog = forecast(m1, h=24)
prog

plot(prog, xlim=c(2012, 2020), main="Tallinki reisijate arvu prognoos")

# Soovi korral võid mängida kunstnikku ja leida kõige meelepärasem graafik prognooside esitamiseks:
prog = forecast(m1, h=24, level=0.95)
plot(prog, xlim=c(2012, 2020), main="Tallinki reisijate arvu prognoos")

prog = forecast(m1, h=24, level=c(0.5, 0.9, 0.95, 0.99))
plot(prog, xlim=c(2012, 2020), main="Tallinki reisijate arvu prognoos", 
  	shadecols=c("gray95", "gray85", "gray80", "gray75") )


# Ning võid lisada graafikule ka hiljem lisandunud reisijate arvud:
plot(prog, xlim=c(2012, 2020), main="Tallinki reisijate arvu prognoos", 
  	shadecols=c("gray95", "gray85", "gray80", "gray75") )
tegelik=c(789272, 852609, 989445, 1223901, 993078, 730631, 785583, 
			654240, 811261, 549278, 620006, 686488, 779113)
teg=ts(tegelik, start=c(2018, 4), frequency=12)
points(teg, col="red", pch=20, cex=2)


# Võid vaadata ka, kuidas erinevad aegrea komponendid
plot(fitted(m1))
plot(stl(y, "per"))


# ARIMA - mudelid

# On veel mudeleid, mille abil on võimalik aegrea struktuuri esitada, 
#  nt ARIMA mudelid. 
# ARIMA ehk AutoRegressive Integrated Moving Average
# R-is on kindel abimees funktsioon `auto.arima()`
y = ts(reisijaid, start=c(2012,5), frequency=12)
m2 = auto.arima(y)
prog = forecast(m2, h=10)
plot(prog, xlim=c(2012, 2020), main="Tallinki reisijate arvu prognoos")





