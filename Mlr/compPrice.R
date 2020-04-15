##Predict Price of the computer
compData<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Mlr\\Computer_Data.csv")
View(compData)

## removing column 1(X)
cmpData<- compData[,-c(1)]
View(cmpData)


## Creating dummy vaiables for discrete data
cmpData$cd<- as.integer(factor(compData$cd,levels=c("yes","no"), labels = c(1,0)))
cmpData$multi<- as.integer(factor(compData$multi,levels = c("yes","no"), labels=c(1,0)))
cmpData$premium<- as.integer(factor(compData$premium, levels = c("yes","no"), labels = c(1,0)))

attach(cmpData)

plot(speed,price)
plot(hd,price)
plot(ram,price)
plot(screen,price)
plot(cd,price)
plot(multi,price)
plot(premium,price)
plot(ads,price)
plot(trend,price)

cor(cmpData)


model1<- lm(price~., data = cmpData)
summary(model1)

model1.sp<- lm(price~speed, data = cmpData)
summary(model1.s)

model1.r<- lm(price~ram, data= cmpData)
summary(model1.r)

model1.h<- lm(price~hd, data = cmpData)
summary(model1.h)

model1.sc<- lm(price~screen,data = cmpData)
summary(model1.sc)

model1.cd<- lm(price~cd, data = cmpData)
summary(model1.cd)

model1.m<- lm(price~multi, data = cmpData)
summary(model1.m)


library(corpcor)
cor(cmpData)

cor2pcor(cor(cmpData))

library(car)

influence.measures(model1)
influenceIndexPlot(model1, id.n=3) # Index Plots of the influence measures
influencePlot(model1, id.n=3)


# removing the influencing data points
model1.Final<- lm(price~., data = cmpData[-c(1441,1701),])
summary(model1.Final)

library("MASS")
avPlots(model1.Final)

##model1.Finalis th best model with R squared value as 0.7777

