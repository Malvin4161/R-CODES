#3prediction model for predicting Price.Corolla
toyota<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Mlr\\ToyotaCorolla.csv")
View(toyota)


corolla<- toyota[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(corolla)


attach(corolla)
summary(corolla)
cor(corolla)

pairs(corolla)

cor2pcor(cor(corolla))

## Builing regression model on corolla data set
model1<- lm(Price ~., data= corolla)
summary(model1)    ## R-squared value - 0.8638

model1$coefficients
model1$residuals

rmse1<-mean(model1$residuals^2)^.5
rmse1  ## rmse - 1338.258

pred1<- predict(model1, corolla)
pred1
cor(pred1, corolla$Price)  ## r - 0.9293

avPlots(model1)

## Checking the influenctial data
influence.measures(model1)
influenceIndexPlot(model1, id.n=3)
influencePlot(model1, id.n=3)  ## data point 81, 961, 222

influence_index <- as.integer(rownames(influencePlot(model1)))

## Building model by removing influential datas
corolla2<- corolla[-c(influence_index),]
model2<- lm(Price~., data = corolla2)
summary(model2)  ## R square value = 0.8852   , so R square value improved

rmse2<- mean(model2$residuals^2)^.5
rmse2    ## rmse value 1227.474, rmse value decreased

pred2<- predict(model2, corolla2)
cor(pred2, corolla2$Price) ## r value 0.94, so it improved

##****** So my model2 has a better R square value with less rmse value and improved r.

