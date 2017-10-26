#Dohvacanje tablice Advertising
Advertising<-read.csv("C:/Users/Denis/Desktop/Seminar Sluzbeno/excel/Advertising.csv",
                      header = TRUE,
                      sep = "," ,
                      dec = "."         
)

View(Advertising)
ad<-Advertising[ ,2:5]
View(ad)


######################      LINEARNA REGRESIJA ####################
#opis varijabli u tablici ad
str(ad)

#graf ovisnosti izmedju sales i TV
plot(sales~TV,data=ad)

#regresijski model
mojaReg=lm(sales~TV,
           data = ad)

#graf regresijskog pravca
abline(mojaReg, col="red")

#sadrzaj regresijskog modela
summary(mojaReg)

#prediktivne vrijednosti
prediktivne<-fitted(mojaReg)
View(prediktivne)
#rezidualne vrijednosti
odstupanja<-residuals(mojaReg)

adRegresija<-cbind(ad,prediktivne, odstupanja)

View(adRegresija)

mojaReg2=lm(sales~TV+radio+newspaper, data=ad)
summary(mojaReg2)

#korelacijska matrica
cor(ad)






