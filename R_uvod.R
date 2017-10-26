###################    KALKULATOR   #############################

2+2

a=2+2

a

sin(3.5)

10^2

50/3
exp(3.7)

###################          LIBRARIES   ###########################


#naredba za ispis svih paketa u osnovnoj R distribuciji 
#(sta sve ima u knjinici)
library()

#naredba za dohvacanje paketa ISLR (nesto kao odjel u knjzinici)
library("ISLR")

#prikazujemo sadrzaj paketa
data(package="ISLR")

#sadrzaj datoteke se dobiva sa
View(Default)
edit(Default)


##################   OSNOVNI TIPOVI VARIJABLI    ################


#numeric
x=10.5
x
class(x)

#integer
y=10
y
class(y)
y=as.integer(10) #integer kreiramo pozivanjem naredba as.integer
y=as.integer(5.7)
y
class(y)

#complex
z=4+5i
z
class(z)


z=4
z
class(z)
z<-as.complex(z)
z
class(z)



#logical
#logicka varijabla se kreira kao rezultate logicke operacije i
#moze poprimiti dvije vrijednosti TRUE, FALSE
x=1
y=2
z=x>y
z
class(z)

#character
x="werb"
x
class(x)

y=as.character(3.22)
y
class(y)

##################   SLOZENI TIPOVI PODATAKA   #################
#####DATA STRUCTURES


#vector
x<-c(3,4,3,2,3,2,5,5,5,5)
x
class(x)

length(x) #duljina vektora

a<-table(x)  #tabularni(frekvencijski) prikaz vektora
a

broj_trojki=sum(x==3)  # broj trojki u vektoru
broj_trojki


#matrix
A = matrix(c(2,4,3,1,5,7), 
           nrow=2,
           ncol=3, 
           byrow=TRUE
           )
A
class(A)


#list



#data frame

v1=c(2,3,5)
v2=c("aa","bb","cc")
v3=c(TRUE,FALSE,TRUE)
df=data.frame(v1,v2,v3)
df
View(df)
class(df)
str(df)

View(Default) #iz paketa ISLR
class(Default)
str(Default)


#####################   UCITAVANJE PODATAKA  ###################

#ucitavanje excel filea u csv formatu (paziti na slash,mora biti forward)
myData<-read.csv("C:/Users/Denis/Desktop/Seminar Sluzbeno/excel/Credit.csv",sep=";",dec=",")
View(myData)
str(myData)

library(gdata)
test<-read.xlsx("C:/Users/Denis/Desktop/test.xlsx", sheet=1)
install.packages("xlsx")
library(xlsx)
test<-read.xlsx("C:/Users/Denis/Desktop/test.xlsx", 1)
View(test)
str(test)

######################   numerièke mjere   #############
#ovo uraditi pod vektore


#######################   GRAFIÈKE MJERE   #####################


#histogram
View(Auto) #iz paketa ISLR
?Auto
h<-hist(Auto$mpg,
        col = "red",
        xlab = "Miles per gallon",
        main = "Histogram potrošnje goriva"
        )

class(h)


#boxplot

boxplot(Auto$mpg ~ Auto$cyl,
        xlab = "Broj cilindara",
        main = "Potrošnja Goriva",
        ylab = "Milja po galonu"
        )

table(Auto$cyl)



#scatterplot


plot(Auto$weight, 
     Auto$mpg,
     main = "Scatterplot primjer",
     xlab = "težina auta",
     ylab = "Milja po galonu"
     )


abline(lm(Auto$mpg ~ Auto$weight), col = "red")







