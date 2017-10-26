
###############crtanje funkcije
#definiramo funkcije
odds_f = function(p){p/(1-p)}
logodds_f = function(p){ log(p/(1-p)) }

p = seq(0.01, 0.99, 0.05)
odds <- odds_f(p)
logodds = logodds_f(p)

plot(p, odds, type="l")
plot(odds, logodds, type="l")







###########################DOHVACANJE DATOTEKE##########################
#instalacija paketa
install.packages("ISLR")

#loadanje paketa
library(ISLR)

#lista datoteka u paketu
data(package="ISLR")

##########################DATOTEKA Default - OSNOVNI PODACI################

#dohvacanje datoteke Default
data(Default)

#opis datoteke
?Default

#tabelarni prikaz datoteke
View(Default)


Default$default = ifelse(Default$default == "Yes", 1, 0)
Default$student = ifelse(Default$student == "Yes", 1, 0)

#pregled tipova varijabli u datoteci
str(Default)

#pretvaramo numericke u factor
Default$default = factor(Default$default)
Default$student = factor(Default$student)

contrasts(Default$default)
contrasts(Default$student)

#################################NUMERICKE I GRAFICKE MJERE#####################

#osnovne numericke mjere varijabli u datoteci
summary(Default)

#jednostavna regresija
mojaLR=glm(default~student,
           data=Default,
           family=binomial)

summary(mojaLR)


#probability

mojPD=predict(mojaLR,type="response")

View(mojPD)

mojPD=cbind(Default["default"], Default["student"], mojPD)
View(mojPD)




#visestruka regresija
mojaVLR=glm(default~student+balance+income,
           data=Default,
           family=binomial)

summary(mojaVLR)


#visestruka regresija
mojaVLR=glm(default~student+balance,
            data=Default,
            family=binomial)

summary(mojaVLR)



###################################################################
#############################BOX PLOT#############################

boxplot(balance ~ student, data=Default, 
        col=(c("gold","darkgreen")))
















