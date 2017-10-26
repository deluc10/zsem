#ucitavanje excel filea u csv formatu (paziti na slash,mora biti forward)
myData<-read.csv("C:/Users/Denis/Desktop/Seminar Sluzbeno/excel/Credit.csv",sep=";",dec=",")

View(myData)
str(myData)

myData$Default = factor(myData$Default)
contrasts(myData$Default)


####################################################################################
########################    UNIVARIJATNA ANALIZA       #############################



#################          osnovne numericke mjere    ####################
summary(myData)
#View(summary(myData))


################    osnovne grafièke mjere   ######################


###############   HISTOGRAMI 
#par(mfrow=c(#rows,#columns))
par(mfrow=c(1,1))
hist(myData$WC.TA,
     main = "Histogram WC.TA",
     xlab = "WC.TA",
     #border ="blue",
     col = "green",
     #prob = TRUE
     )
lines(density(myData$WC.TA))

hist(myData$RE.TA)
hist(myData$EBIT.TA)
hist(myData$ME.TL)
hist(myData$S.TA)


##################   BOX PLOTOVI       
boxplot(WC.TA~Default, 
        data=myData,
        #outline = FALSE,
        col=c("gold","darkgreen")
        )

boxplot(RE.TA~Default, 
        data=myData, 
        #outline = FALSE, 
        col=c("gold","darkgreen")
        )

boxplot(EBIT.TA~Default, data=myData, 
        col=(c("gold","darkgreen")))

boxplot(ME.TL~Default, data=myData, 
        col=(c("gold","darkgreen")))

boxplot(S.TA~Default, data=myData, 
        col=(c("gold","darkgreen")))


####################   BOX PLOTOVI ZAJEDNO    
install.packages("reshape2")
library(reshape2)
myData<-myData[,c(3,4,5,6,7,8)]
myDataMelt<-melt(myData,id.var="Default")
View(myDataMelt)
str(myDataMelt)

#myDataMelt$Default<-factor(myDataMelt$Default)

levels(myDataMelt$Default)

edit(myDataMelt)

install.packages("ggplot2")
library(ggplot2)
p<-ggplot(data = myDataMelt, aes(x=variable, y=value,fill=Default)) + 
  geom_boxplot()
p<-p+facet_wrap(~variable,scales="free")
p


#p <- p + geom_jitter()
p <- p + xlab("x-axis") + ylab("y-axis") + ggtitle("Title")
p <- p + guides(fill=guide_legend(title="Legend_Title"))
p



###############    LOGISTICKA REGRESIJA MCFADEN R2
install.packages("rms")
library(rms)
str(myData)
lrm(formula = Default ~ WC.TA, data = myData)
lrm(formula = Default ~ RE.TA, data = myData)
lrm(formula = Default ~ EBIT.TA, data = myData)
lrm(formula = Default ~ ME.TL, data = myData)
lrm(formula = Default ~ S.TA, data = myData)


lrm(formula = Default ~ WC.TA+RE.TA+EBIT.TA+ME.TL+S.TA, data = myData)


##########woe i information value
install.packages("Information")
library(Information)
install.packages("gridExtra")
library(gridExtra)
install.packages("grid")
library(grid)

data(package="Information")
View(train)
dim(train)
train<-subset(train, TREATMENT==1)
str(train)
IV<-Information::create_infotables(data=train,y="PURCHASE")

#IV vrijednosti za svaku varijablu
#IV$Summary
View(IV$Summary)
grid.newpage()
grid.table(head(IV$Summary), rows = NULL)
grid.table(IV$Summary[1:10,], rows = NULL)

#woe za zadanu varijablu
View(IV$Tables$N_OPEN_REV_ACTS)
grid.table(IV$Tables$N_OPEN_REV_ACTS, rows=NULL)

#crtanje woe-a
SinglePlot(IV, "N_OPEN_REV_ACTS", show_values = TRUE)
plot_infotables(IV, "N_OPEN_REV_ACTS",
                same_scales = FALSE, show_values = FALSE)
#http://multithreaded.stitchfix.com/blog/2015/08/13/weight-of-evidence/



###############   INFORMATION VALUE for myData
View(myData)
str(myData)
myDataIV<-myData
myDataIV$Default<-as.numeric(myDataIV$Default)
myDataIV<-myDataIV[ ,3:8]
str(myDataIV)
View(myDataIV)

#myDataIV$Default<-ifelse(myDataIV$Default==1, 0, 1)
IV<-Information::create_infotables(data=myDataIV,y="Default")
View(myDataIV)

View(IV$Summary)
grid.newpage()
grid.table(head(IV$Summary), rows = NULL)
grid.table(IV$Summary, rows = NULL)

#woe za zadanu varijablu
View(IV$Tables$ME.TL)
grid.table(IV$Tables$ME.TL, rows=NULL)

#crtanje woe-a
SinglePlot(IV, "ME.TL", show_values = TRUE)
plot_infotables(IV, "ME.TL",
                same_scales = FALSE, show_values = FALSE)





#############################################################################
############################    MULTIVARIJATNA ANALIZA   ###############################


  
########################    MODEL 
glm.fit=glm(Default ~ WC.TA + RE.TA + EBIT.TA + ME.TL + S.TA, data = myData, family = binomial)
summary(glm.fit)
#anova(glm.fit)

#######################   FORWARD BACKWARD HYBRID

fullModel=glm(Default~WC.TA + RE.TA + EBIT.TA + ME.TL + S.TA, data = myData, family = binomial)
summary(fullModel)

nullModel=glm(Default~1,data = myData, family = binomial)
summary(nullModel)

backwards=step(fullModel)
summary(backwards)

forwards=step(nullModel, scope=list(lower=formula(nullModel),upper=formula(fullModel)), direction="forward")
summary(forwards)

bothways=step(nullModel, scope=list(lower=formula(nullModel),upper=formula(fullModel)), direction="both")
summary(bothways)       

myModel=glm(Default~RE.TA + EBIT.TA + ME.TL + S.TA, data = myData, family = binomial)
summary(myModel)

class(bothways)

#################   IZRAÈUN VJEROJATNOSTI
View(myData)

PD=predict(bothways,type="response")

View(PD)

myDataPD = cbind(myData, PD)

View(myDataPD)


#####################################################################################
##########################    TESTIRANJE    #######################################

##################  MATRICA KONFUZIJE 


klasifikacija=rep(0,4000)
View (klasifikacija)
klasifikacija[PD>0.05]=1
View(klasifikacija)

myDataPD=cbind(myDataPD, klasifikacija)
View(myDataPD)



#confusion matrix
table(klasifikacija,myData$Default)



######################   ROC KRIVULJA  
#https://stackoverflow.com/questions/11467855/roc-curve-in-r-using-rocr-package


library(ROCR)

#informacije izvedene iz confusion matrix i puno vise
pred=prediction(myDataPD$PD,myData$Default)
summary(pred)
View(pred)


perf=performance(pred,"tpr","fpr")
plot(perf)

#povrsina ispod roc krivulje
auc=performance(pred,"auc")
auc





