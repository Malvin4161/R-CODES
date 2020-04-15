##1. predict weight gained using calories consumed
Consumed<-read.csv(choose.files())


## Performing EDA
getwd()
setwd("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Slr\\Assignments")
?lattice

library(lattice)
dotplot(Consumed$Weight.gained..grams.)
dotplot(Consumed$Calories.Consumed)
boxplot(Consumed$Weight.gained..grams.)
boxplot(Consumed$Calories.Consumed)
hist(Consumed$Weight.gained..grams.)
hist(Consumed$Calories.Consumed)

qqnorm(Consumed$Weight.gained..grams.)
qqline(Consumed$Weight.gained..grams.)

qqnorm(Consumed$Calories.Consumed)
qqline(Consumed$Calories.Consumed)

#Scatter plot
plot(Consumed$Weight.gained..grams.,Consumed$Calories.Consumed,main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Calorie consumed", 
     ylab="Weight gained", pch=20)


attach(Consumed)

##checking the correlation
cor(Weight.gained..grams.,Calories.Consumed)


##creating the regression model
reg<- lm(Weight.gained..grams.~Calories.Consumed, data= Consumed)
summary(reg)

# R squared value is 0.89 which is a strong model.

reg$coefficients
reg$residuals

pred<- predict(reg,interval="confidence")
pred<- as.data.frame(pred)
pred

x<- predict(reg,interval="predict")
x<-as.data.frame(x)
x

sqrt(sum(reg$residuals^2)/nrow(Consumed))

cor(pred$fit,Consumed$Weight.gained..grams.)

confint(reg,level=0.95)
predict(reg,interval="predict")

plot(x,Consumed$Calories.Consumed)

library(ggplot2)
ggplot(data = Consumed,mapping=aes(x = Calories.Consumed,y=Weight.gained..grams.))+geom_smooth(method = "lm")+geom_point()+ggtitle("Model 1",subtitle = "Y~X")

##********************************************************##
##2Predict delivery time using sorting time 

Delivery_time<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Slr\\Assignments\\delivery_time.csv")

##EDA
dotplot(Delivery_time$Delivery.Time)
dotplot(Delivery_time$Sorting.Time)
hist(Delivery_time$Delivery.Time)
hist(Delivery_time$Sorting.Time)

qqnorm(Delivery_time$Delivery.Time)
qqline(Delivery_time$Delivery.Time)

qqnorm(Delivery_time$Sorting.Time)
qqline(Delivery_time$Sorting.Time)

plot(Delivery_time$Sorting.Time,Delivery_time$Delivery.Time,main = "Scatterplot", col.main="Dodgerblue4",
     col.lab="Red",col="green",ylab="Delivery time",xlab="sorting time",pch=20)
##correlation
cor(Delivery_time$Delivery.Time,Delivery_time$Sorting.Time)

##regression model
reg<-lm(Delivery_time$Delivery.Time~Delivery_time$Sorting.Time)
summary(reg)

reg$coefficients
reg$residuals

pred<-predict(reg,interval="confidence")
pred<-as.data.frame(pred)
pred

x<-predict(reg,interval="predict")
x<-as.data.frame(x)
x


sqrt(sum(reg$residuals^2)/nrow(Delivery_time)) ## RMSE


##Transforming in sqrt of sorting time
reg_sqrt<-lm(Delivery_time$Delivery.Time ~ sqrt(Delivery_time$Sorting.Time))
summary(reg_sqrt)     
cor(sqrt(Delivery_time$Sorting.Time),Delivery_time$Delivery.Time)
plot(sqrt(Delivery_time$Sorting.Time),Delivery_time$Delivery.Time)


#sqrt of delivery time
reg_sqrt1<-lm(sqrt(Delivery_time$Delivery.Time) ~ Delivery_time$Sorting.Time)
summary(reg_sqrt1)
cor(sqrt(Delivery_time$Delivery.Time),Delivery_time$Sorting.Time)
plot(Delivery_time$Delivery.Time, sqrt(Delivery_time$Sorting.Time))

confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

##Log transformation


reg_log<- lm(Delivery_time$Delivery.Time ~ log(Delivery_time$Sorting.Time))
summary(reg_log)

reg_log1<-lm(log(Delivery_time$Delivery.Time) ~ Delivery_time$Sorting.Time + I(Delivery_time$Sorting.Time*Delivery_time$Sorting.Time))
summary(reg_log)


## SO sqrt of delivery_time is the best transformation as R squared value is 0.70



##*********************************************************************
#3 Build a prediction model for Churn_out_rate

EmpData<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Slr\\Assignments\\emp_Data.csv")

##EDA

dotplot(EmpData$Salary_hike)
dotplot(EmpData$Churn_out_rate)

hist(EmpData$Salary_hike)
hist(EmpData$Churn_out_rate)


qqnorm(EmpData$Salary_hike)
qqline(EmpData$Salary_hike)


qqnorm(EmpData$Churn_out_rate)
qqline(EmpData$Churn_out_rate)

plot(EmpData$Churn_out_rate,EmpData$Salary_hike, main = "Scatter plot",col="yellow",
     col.main="blue",col.lab="blue", xlab="churnout",ylab="salary_hike", pch=20)

cor(EmpData$Churn_out_rate,EmpData$Salary_hike)

reg<-lm(Salary_hike~Churn_out_rate, data=EmpData)
summary(reg)

reg$coefficients
reg$residuals

reg_sqrt<-lm(Salary_hike~sqrt(Churn_out_rate), data=EmpData)
summary(reg_sqrt)

cor(EmpData$Churn_out_rate,sqrt(EmpData$Salary_hike))

reg_sqrt1<-lm(sqrt(Salary_hike)~Churn_out_rate, data=EmpData)    
summary(reg_sqrt1)


reg_log<- lm(Salary_hike~log(Churn_out_rate),data=EmpData)
summary(reg_log)
#  R-squaredvalue is better in log transform.
reg_log$coefficients
reg_log$residuals

cor(EmpData$Salary_hike,log(EmpData$Churn_out_rate))
plot(EmpData$Salary_hike,log(EmpData$Churn_out_rate))

sqrt(sum(reg_log$residuals^2)/nrow(EmpData))


confint(reg_log,level=0.95)
predict(reg_log,interval="predict")


##*****************************************************************
#4Build a prediction model for Salary_hike
SalaryHike<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Slr\\Assignments\\Salary_Data.csv")


hist(SalaryHike$YearsExperience)
hist(SalaryHike$Salary)

qqnorm(SalaryHike$YearsExperience)
qqline(SalaryHike$YearsExperience)

qqnorm(SalaryHike$Salary)
qqline(SalaryHike$Salary)


plot(SalaryHike$YearsExperience,SalaryHike$Salary, main="scatter_plot", col.main="blue",
     col="green", xlab="yearsOfexp", ylab="Salary",pch=20)

attach(SalaryHike)
cor(Salary,YearsExperience)

reg <- lm(Salary ~ YearsExperience, data = SalaryHike)
summary(reg)


reg$coefficients
reg$residuals

pred <- predict(reg,interval="confidence")

pred <- as.data.frame(pred)
pred
x = predict(reg,interval = "predict")
x  = as.data.frame(x)
x


sqrt(sum(reg$residuals^2)/nrow(SalaryHike)) ## RMSE 
cor(pred$fit, SalaryHike$Salary)

confint(reg,level=0.95)
predict(reg,interval="predict")
