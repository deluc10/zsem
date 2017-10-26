#ucitavanje excel filea u csv formatu (paziti na slash,mora biti forward)
myData<-read.csv("C:/Users/Denis/Desktop/Seminar Sluzbeno/excel/Credit.csv",sep=";",dec=",")


View(myData)
str(myData)

myData$Default = factor(myData$Default)
contrasts(myData$Default)

summary(myData)

train<-sample(1:nrow(myData),3000,replace=FALSE)
train<-sort(train)
View(train)

test<-setdiff(c(1:4000),train)
View(test)

myData.train<-myData[train, ]
myData.test<-myData[test, ]
dim(myData.train)
dim(myData.test)
View(myData.train)
View(myData.test)


###################################################################################
##########################   DESCRIPTIVE STATISTICS   ###############################



#################          osnovne numericke mjere  
summary(myData.train)
#View(summary(myData))


################    osnovne grafièke mjere   


#######################    HISTOGRAMI   

#two or more plots in the sam window
#par(mfrow=c(#rows,#columns))
par(mfrow=c(1,1))
hist(myData.train$WC.TA,
     main = "Histogram WC.TA",
     xlab = "WC.TA",
     #border ="blue",
     col = "green",
     prob = TRUE
)
lines(density(myData.train$WC.TA))

hist(myData.train$RE.TA)
hist(myData.train$EBIT.TA)
hist(myData.train$ME.TL)
hist(myData.train$S.TA)


##########################   BOX PLOTOVI  
boxplot(WC.TA~Default, 
        data=myData.train,
        #outline = FALSE,
        col=c("gold","darkgreen")
)

boxplot(RE.TA~Default, 
        data=myData.train, 
        #outline = FALSE, 
        col=c("gold","darkgreen")
)

boxplot(EBIT.TA~Default, data=myData.train, 
        col=(c("gold","darkgreen")))

boxplot(ME.TL~Default, data=myData.train, 
        col=(c("gold","darkgreen")))

boxplot(S.TA~Default, data=myData.train, 
        col=(c("gold","darkgreen")))


##########################   BOX PLOTOVI ZAJEDNO    
library(reshape2)
myData.train<-myData.train[,c(3,4,5,6,7,8)]
myDataMelt<-melt(myData.train,id.var="Default")
View(myDataMelt)
str(myDataMelt)

#myDataMelt$Default<-factor(myDataMelt$Default)

levels(myDataMelt$Default)

edit(myDataMelt)

library(ggplot2)
p<-ggplot(data = myDataMelt, aes(x=variable, y=value,fill=Default)) + 
  geom_boxplot()
p<-p+facet_wrap(~variable,scales="free")
p


#p <- p + geom_jitter()
p <- p + xlab("x-axis") + ylab("y-axis") + ggtitle("Title")
p <- p + guides(fill=guide_legend(title="Legend_Title"))
p







#############################################################################
############################    MODELIRANJE   ###############################



#################   MODEL         
glm.fit=glm(Default ~ WC.TA + RE.TA + EBIT.TA + ME.TL + S.TA, data = myData.train, family = binomial)

summary(glm.fit)

#anova(glm.fit)

#################  FORWARD BACKWARD


fullModel=glm(Default~WC.TA + RE.TA + EBIT.TA + ME.TL + S.TA, data = myData, family = binomial,subset=train)
summary(fullModel)

nullModel=glm(Default~1,data = myData, family = binomial, subset = train)
summary(nullModel)

backwards=step(fullModel)
summary(backwards)

forwards=step(nullModel, scope=list(lower=formula(nullModel),upper=formula(fullModel)), direction="forward")
summary(forwards)

bothways=step(nullModel, scope=list(lower=formula(nullModel),upper=formula(fullModel)), direction="both")
summary(bothways)


class(bothways)


myModel=glm(Default~RE.TA + EBIT.TA + ME.TL + S.TA, data = myData, family = binomial,subset=train)
summary(myModel)

################          IZRAÈUN VJEROJATNOSTI
nrow(myData.train)
View(myData.test)
nrow(myData.test)
sum(train)
PD=predict(myModel,myData.test, type="response")
length(PD)

nrow(myData.test)

myDataPD = cbind(myData.test, PD)

View(myDataPD)



########################    MATRICA KONFUZIJE  
klasifikacija=rep(0,nrow(myDataPD))
View (klasifikacija)
klasifikacija[PD>0.05]=1
View(klasifikacija)

myDataPD=cbind(myDataPD, klasifikacija)
View(myDataPD)



#confusion matrix
table(klasifikacija,myDataPD$Default)



###########################   ROC KRIVULJA    
library(ROCR)

#informacije izvedene iz confusion matrix i puno vise
pred=prediction(myDataPD$PD,myDataPD$Default)
summary(pred)
View(pred)

#grafièki prikaz roc krivulje
perf=performance(pred,"tpr","fpr")
plot(perf)

#povrsina ispod roc krivulje
auc=performance(pred,"auc")
auc

