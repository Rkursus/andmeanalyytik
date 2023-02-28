# Andmeanalüütiku arenguprogramm
# 6. õppepäev
# Statistilised meetodid praktilises elus
#
# Klientide segmenteerimine
# Autor: Kaur Lumiste


## Ettevalmistused ----

# Kui soovid koolitust oma arvutiga kaasa teha, siis esmalt paigalda Rstudios 
#   järgmised paketid. 
install.pacakges('readxl')
install.packages('dplyr')
install.packages('corrplot')

# Ja laeme paketid töömällu järgmiselt
library(readxl)
library(dplyr)
library(corrplot)

# NIPP - Töökausta seadistamine - 
#    töökaustaks seatakse automaatselt kaust kus asub käesolev fail
#install.packages('rstudioapi')
setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))


## Sissejuhatus ----

# Klientide segmenteerimiseks (või ükskõik milliste andmete lahterdamiseks) kasutame k-keskmiste meetodit (_k-means_). 
#  Käime läbi järgmised etapid:

# Andmetega tutvumine
# Andmete puhastamine
# Andmete rikastamine
# Uute andmete eraldamise, agregeerime
# Teostame segmenteerimise
# Analüüsime tulemusi

# Näidis analüüsi tegemiseks kasutame vabalt kättesaadavat andmestikku 
#  [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/online+retail#) 
#   veebilehelt, mis on omakorda saanud andmed 
#   [Chen, Sain & Guo (2012)](https://link.springer.com/article/10.1057/dbm.2012.17) artiklist. 

# Andmesikus on Suurbritannias registreeritud veebipoe tehinguandmed vahemikus 
#  1. detsember 2010 - 9.  detsember 2011. Veebipoes müüakse enamasti kingitusi 
#  ja paljud veebipoe kliendid on jae- või hulgimüüjad.

# Andmestiku saate sisse laadida järgmise käsu abil:
# NB! pead ise määrama faili täpse asukoha failipuus või töökaustas
jaemyyk <- read_excel("../data/jaemyyk.xlsx", 
     col_types = c("text", "text", "text", 
          "numeric", "date", "numeric", "text", 
          "text"))

# Või lihtsam variant lugeda sisse URLi kaudu (nõuab aga teist paketti):
# NB! peab teadma faili URLi
#install.packages('httr')
library(httr)

url = "https://github.com/Rkursus/andmeanalyytik/raw/master/data/jaemyyk.xlsx"
GET(url, write_disk(file <- tempfile(fileext = ".xlsx")))
jaemyyk <- read_excel(file, 
                      col_types = c("text", "text", "text", 
                                    "numeric", "date", "numeric", "text", 
                                    "text"))



## Andmeväljad ja andmestikuga tutvumine ----

# Kui andmestikuga kaasa tulnud tunnuste kirjeldused on järgmised:

# arve_nr - Arve number. Nominaalne tunnus, 6-kohaline arv, mis on iga 
#   tehingu puhul unikaalne. Kui kood algab 'c' tähega, siis indikeerib see tühistatud tehingut.
# kood - Toote või kauba kood. Nominaalne tunnus, 5-kohaline unikaalne number iga toote kohta.
# kirjeluds - Toote või kauba kirjeldus. Nominaalne tunnus.
# kogus - Iga toote või kauba kogus, mis osteti käesoleval tehingul. Numbriline tunnus.
# ostu_kp - Tehingu toimumise kuupäev ja kellaaeg.
# yhiku_hind - Toote või kauba ühiku hind. Numbriline, valuuta GBP.
# kliendi_id - Unikaalne kliendi ID. Nominaalne tunnus, 5-kohaline number.
# riik - riigi nimi. Nominaalne tunnus, riik, kus klient resideerub.

# Veendumaks, et kõik tunnused on olemas 
head(jaemyyk)
str(jaemyyk)
summary(jaemyyk)


# Saab genereerida järgmise väga põhjaliku:
library(DataExplorer)
DataExplorer::create_report(jaemyyk)



## Andmete puhastamine ----

jaemyyk <- jaemyyk %>% 
  filter(!is.na(kliendi_id), # tehingud millel puudub kliendi ID
         kogus > 0,          # ainult kogus > 0
         yhiku_hind > 0,     # Ühiku hind olgu alati positiivne
         substr(arve_nr,1,1) != 'C', # Eemaldame tühistatud tehingud
         kogus < 3000,       # Ebamõistlikult suured kogused
         yhiku_hind < 3000   # Väga kallid tooted jätame kõrvale
         )

summary(jaemyyk)

## Rikastame andmeid ----

# Leiame ostu summa ja ostu sooritamise kuu
jaemyyk <- jaemyyk %>% 
  mutate(myyk = yhiku_hind * kogus,
         ostukuu = months(ostu_kp))


## Agregeerime klientide andmed ----

# Grupeerime andmed esmalt kliendi kaupa ja agregeerime
kliendid <- jaemyyk %>% 
  group_by(kliendi_id) %>% 
  summarise(kaive = sum(myyk),
            tehinguid = length(unique(arve_nr)), # unikaalsete tehingute arv
            kesk_ost = kaive / tehinguid)



# Segmenteerimine k-keskmiste meetodi abil ----


## k-keskmiste klasterdamise ettevalmistused ----

# Logaritm säilitab väärtuste järekorra ja hiljem kui on vaja tõlgendada, 
#  siis saame kasutada pöördfunktsiooni ehk eksponenti `exp()`.
#  Ühtlasi töötab _k_-keskmiste klasterdamine standardiseeritud andmetega. 
 
kliendid <- kliendid %>% 
  mutate(log_kaive = log(kaive),
         log_tehinguid = log(tehinguid),
         log_kesk_ost = log(kesk_ost))

skaleeritud <- kliendid %>% 
  select(log_kaive, log_tehinguid, log_kesk_ost) %>% 
  scale() 


## Klastrite arvu _k_ määramine ----

# Kasutame nn. "küünarnuki" meetodit (_elbow method_). 
# Eelnevad suurused, mis on vaja defineerida
k = 1:20
pr = 1

# Proovime läbi kõik klasterdamised klastriarvuga 1st-20ni.
for (i in 2:20){
  km = kmeans(skaleeritud,i)
  pr[i] = km$tot.withinss/km$totss
}

# Küünarnuki joonis
plot(k,pr,type="b")




## Klientide segmenteerimine ----


# Klasterdamer ja leiame klastrite keskmised (ehk keskpunkti)
klastrid = kmeans(skaleeritud,4)
klastrid$centers

# Joonis, mis hõlbustab analüüsimist
corrplot(klastrid$centers, is.corr=F, main = 'Klastrite keskmised')



# Võime veenduda oma tõlgendustes järgmiste abistavate jooniste abil:
plot(kliendid$log_kaive, kliendid$log_tehinguid, col = klastrid$cluster, pch = 20,
     xlab = 'Käive',
     ylab = 'Tehingute arv',
     main = 'Käibe ja tehingute arvu hajuvusdiagramm')

plot(kliendid$log_tehinguid, kliendid$log_kesk_ost, col = klastrid$cluster, pch = 20,
     xlab = 'Tehingute arv',
     ylab = 'Keskmine ostusumma',
     main = 'Tehingute arvu ja keskmise ostusumma hajuvusdiagramm')

plot(kliendid$log_kaive, kliendid$log_kesk_ost, col = klastrid$cluster, pch = 20,
     xlab = 'Käive',
     ylab = 'Keskmine ostusumma',
     main = 'Käibe ja keskmise ostusumma hajuvusdiagramm')

### Ülesanne nuputamiseks
# Eelmiseid jooniseid on kohmakas vaadata, sest saame jooniseid teha vaid kahe tunnuse kaupa. 
#  Rakenda eelmisel õppepäeval õpitud multidimensionaalset skaleerimist (isoMDS).



# Kui nüüd soovime teada millised kliendid kuuluvad teatud klastrisse 
#  (et siis vastavalt adresseerida või nt turundust suunata nendele), 
#   siis saame klientide ID-d eraldada järgmiste käskude abil:
kliendid$kliendi_id[klastrid$cluster == 1] # 1. klastrisse kuuluvad kliendid
kliendid$kliendi_id[klastrid$cluster == 2] # 2. klastrisse kuuluvad kliendid
# jne



