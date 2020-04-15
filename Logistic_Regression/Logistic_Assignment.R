##1Whether the client has subscribed a term deposit or not Binomial ("yes" or "no")
Bank_Data<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Logistic_Regression\\bank-full.csv")
getwd()
setwd("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Logistic_Regression")
View(Bank_Data)
str(Bank_Data)

attach(Bank_Data)

##EDA
plot(y,age)
plot(y, job)
plot(y,marital)
plot(y,education)
plot(y,default)
plot(y,balance)
plot(y,housing)
plot(y,loan)
plot(y,day)
plot(y, month)
plot(y, duration)
plot(y,pdays)
plot(y,previous)
plot(y, campaign)
plot(y,poutcome)

boxplot(Bank_Data)

summary(Bank_Data)



##Split the data into train and test data to test the regression model
library(caTools)
set.seed(101)
sample<- sample.split(Bank_Data$age, SplitRatio = 0.75)
train<-subset(Bank_Data, sample== TRUE)
test<- subset(Bank_Data, sample== FALSE)


## Builing the logistic model 
logistic<-glm(y~., family= "binomial",data = train)
summary(logistic)


## confusion matrix table using test data

?predict
prob= predict(logistic, type = c("response"), test)
prob

confusion<- table(prob>0.5, test$y)
confusion


## Checking the model accuracy
Accuracy<- sum(diag(confusion))/sum(confusion)
Accuracy  ## This shows my model has 90 % chance of correct prediction on y.

pred_values=NULL
yes_no= NULL

for (i in 1:45211)
{
  pred_values= ifelse(prob[i]>=0.5,1,0)
  yes_no= ifelse(prob[i]>=0.5,"yes","no")
}

test[,"probabilities"]= prob
test[,"predicted_Values"]= pred_values
test[,"yes_no"]= yes_no


## Checking the ROC curve
library(ROCR)
?prediction
rocrpred<- prediction(prob,test$y)
rocrperf<- performance(rocrpred,'tpr','fpr')
rocrperf
plot(rocrperf,colorize=T)

##****************************************************************************
## 2.Classify whether application accepted or not using Logistic regression
creditApp<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Logistic_Regression\\creditcard.csv")
View(creditApp)
str(creditApp)

App<- creditApp[,-c(1)]  ## removing column 1 as its nt required
View(App)

##EDA

attach(App)
plot(card, reports)
plot(card, age)
plot(card, income)
plot(card, share)
plot(card, expenditure)
plot(card, owner)
plot(card,selfemp)
plot(card, dependents)
plot(card, months)
plot(card, majorcards)
plot(card, active)


boxplot(App)
summary(App)
str(App)

##Split the data into train and test data to test the regression model
library(caTools)
set.seed(101)
sample<- sample.split(Bank_Data$age, SplitRatio = 0.75)
train<-subset(Bank_Data, sample== TRUE)
test<- subset(Bank_Data, sample== FALSE)


## Building the regression model
logisR<- glm(card~., family= "binomial",maxit=1000,data = train )
summary(logisR)


## Confusion matrix
prob<- predict(logisR,type = c("link"), test)
prob

confusion<- table(prob>0.5, test$card)
confusion

# Accuracy
Accuracy<- sum(diag(confusion))/sum(confusion)
Accuracy  ## model shows 98 % accuracy


pred_values=NULL
yes_no= NULL

for (i in 1:1319)
{
  pred_values= ifelse(prob[i]>=0.5,1,0)
  yes_no= ifelse(prob[i]>=0.5,"yes","no")
}

test[,"probabilities"]= prob
test[,"predicted_Values"]= pred_values
test[,"yes_no"]= yes_no
## ROC curve
library(ROCR)
rocpred<- prediction(prob,test$card)
rocperf<- performance(rocpred,'tpr','fpr')
plot(rocperf, colorize=T)
