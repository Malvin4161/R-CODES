##Prepare a prediction model for profit of 50_startups data
startups<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Mlr\\50_Startups.csv") 
getwd()
setwd("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Mlr")
View(startups)
str(startups)

# crating dummy variable for state
startups$State<- as.integer(factor(startups$State, levels = c('New York','California','Florida'),labels = c(1,2,3)))
View(startups)
attach(startups)

summary(startups)



##EDA
hist(R.D.Spend)
hist(Administration)
hist(Marketing.Spend)
hist(Profit)
hist(State)

qqnorm(R.D.Spend)
qqline(R.D.Spend)

qqnorm(Administration)
qqline(Administration)

qqnorm(Marketing.Spend)
qqline(Marketing.Spend)


plot(R.D.Spend,Profit)
plot(Administration,Profit)
plot(Marketing.Spend,Profit)
plot(State,Profit)


pairs(startups)

#correlation in all variables
cor(startups)

##Regression model1
model1<- lm(Profit~., data = startups)
summary(model1)

# in model1 R.D and Marketing shows better significance

## Check individual significance

model1.RD<- lm(Profit~R.D.Spend)
summary(model1.RD)

model1.Ad<- lm(Profit~Administration)
summary(model1.Ad)


model1.Ms<- lm(Profit~Marketing.Spend)
summary(model1.Ms)


## Check the partial correlation of all variables
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}

pairs(startups, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")


###                                   Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor(startups)

cor2pcor(cor(startups))

plot(model1)

qqPlot(model1, id.n=5) 

# Deletion Diagnostics for identifying influential variable
library(car)
influence.measures(model1)

influenceIndexPlot(model1, id.n=3) # Index Plots of the influence measures
influencePlot(model1, id.n=3) # A user friendly representation of the above

## Regression after deleting the 56th observation
model1.r1<-lm(Profit~., data=startups[-50])
summary(model1.r1)

## Regression after deleting the 46 and 56th observation
model1.r2<-lm(Profit~., data=startups[-c(46,50),])
summary(model1.r2)

## As removing 46 50 number data makes the modelbetter


### Variance Inflation Factors
vif(model1)  # VIF is > 10 => collinearity
VIFState<-lm(Profit~ R.D.Spend+Administration+Marketing.Spend, data = startups[-c(46,56)])
VIFAdmin<-lm(Profit~ R.D.Spend+Marketing.Spend, data = startups[-c(46,50),])

summary(VIFState)
summary(VIFAdmin)



avPlots(model1.r2, id.n=2, id.cex=0.8, col="red")

## model1.r2 shows the better result with R^2 value as .9635 


