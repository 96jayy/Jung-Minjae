data(airquality)
str(airquality)
head(airquality)
a<-airquality[airquality$Month==8,]
a[a$Day==20,]

data(iris)
head(iris)
mean(iris$Sepal.Length)+mean(iris$Sepal.Width)

data(mtcars)
head(mtcars)

nrow(mtcars[mtcars$cyl==4,])/nrow(mtcars)*100

a<-mtcars[mtcars$gear==4&mtcars$am==1,]
mean(a$mpg)+sd(a$hp)

library(MASS)
data("Boston")
head(Boston)
mean(Boston[Boston$crim<=1,]$medv)

head(iris)
dim(iris)[1]
len<-rep(0,dim(iris)[1])
iris<-cbind(iris,len)
iris[iris$Species=="virginica"&iris$Sepal.Length>6,]$len<-1
sum(iris$len)

head(airquality)

for(i in 1:dim(airquality)[1]){
  if(is.na(airquality$Ozone[i])==T)
    airquality$Ozone[i]<-mean(airquality$Ozone,na.rm=T)
}
low<-median(airquality$Ozone)-2*IQR(airquality$Ozone)
up<-median(airquality$Ozone)+2*IQR(airquality$Ozone)

sum(airquality[airquality$Ozone>low & airquality$Ozone<up,]$Ozone)

data("ChickWeight")
head(ChickWeight)
train<-ChickWeight[ChickWeight$Time==10,]
sort(train$weight,decreasing=T)[30]
mb<-mean(train$weight)

train[train$weight>=103,]$weight<-mean(train$weight)
ma<-mean(train$weight)
mb-ma

install.packages("ISLR")
library(ISLR)
data("Hitters")
head(Hitters)
iq<-IQR(Hitters$Salary,na.rm=T)
med<-median(Hitters$Salary,na.rm=T)
sum(Hitters[Hitters$Salary>med+2*iq | Hitters$Salary<med-2*iq,]$Salary,na.rm=T)

library(ggplot2)
data("diamonds")
head(diamonds)
i<-nrow(diamonds)*0.8
train<-diamonds[1:i,]
mean(sort(train$price,decreasing = T)[1:100])

data(cats)
up<-mean(cats$Hwt)+1.5*sd(cats$Hwt)
low<-mean(cats$Hwt)-1.5*sd(cats$Hwt)

mean(cats[cats$Hwt>up | cats$Hwt<low,]$Hwt)


install.packages("faraway")
library(faraway)
data(orings)
head(orings)
cor(orings[orings$damage>=1,]$temp,orings[orings$damage>=1,]$damage)

data(diamonds)
ds<-diamonds[i:nrow(diamonds),]
max(ds[ds$cut=="Fair" & ds$carat>=1,]$price)


library(caret)
b<-preProcess(mtcars,method=c("range"))
b<-predict(b,mtcars)
sum(b$qsec>0.5)

install.packages("randomForest")
library(randomForest)
randomForest()