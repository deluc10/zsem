###################   STABLO ODLUÈIVANJA  ###################
install.packages("tree")
library(tree)

library(ISLR)

data(package="ISLR")
View(Carseats)

str(Carseats)
?Carseats
High=ifelse(Carseats$Sales<=8, "No", "Yes")
View(High)
Carseats = data.frame(Carseats, High)
View(Carseats)


##############   KREIRAMO STABLO
tree.carseats = tree(High ~ . - Sales, 
                     data = Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats)

tree.pred = predict(tree.carseats, Carseats, , type="class")
View(tree.pred)
table(tree.pred, Carseats$High)


################   KREIRAMO STABLO PAKETIMA rpart i rpart.plot
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


binary.model<-rpart(High ~ . - Sales, 
                    data = Carseats, method="class")

rpart.plot(binary.model)

probabilities<- predict(binary.model, data = Carseats, type = "prob")
dim(Carseats)
View(probabilities)


View(binary.model)
#########################   PRIMJER 2 

#ucitavanje excel filea u csv formatu (paziti na slash,mora biti forward)
myData<-read.csv("C:/Users/Denis/Desktop/Seminar Sluzbeno/excel/Credit.csv",sep=";",dec=",")


View(myData)
str(myData)

myData$Default = factor(myData$Default)
contrasts(myData$Default)

myData<-myData[,3:8]
summary(myData)

tree.myData = tree(Default ~ .,
                   data = myData)


plot(tree.myData)

text(tree.myData)

myData.model<-rpart(Default ~ ., 
                    data = myData, method="class")

rpart.plot(myData.model)

