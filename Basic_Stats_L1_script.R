##Question 7
Q7 <- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Basic_Statistics_level_1\\Q7.csv")
mean(Q7$Points)
mean(Q7$Score)
mean(Q7$Weigh)
median(Q7$Points)
median(Q7$Score)
median(Q7$Weigh)
install.packages("modeest")
library("modeest")
??mlv
mlv(Q7$Points,method="mfv")
mlv(Q7$Score,method="mfv")
mlv(Q7$Weigh,method="mfv")

## variance,standard deviation and range
var(Q7$Points)
var(Q7$Score)
var(Q7$Weigh)

sd(Q7$Points)
sd(Q7$Score)
sd(Q7$Weigh)

range(Q7$Points)# this gives the max and min value of the dataset, so now make a function 
# to calculate th range
rangevalue<- function(x){max(x)-min(x)}
rangevalue(Q7$Points)
rangevalue(Q7$Score)
rangevalue(Q7$Weigh)

##Question 8
weight<- c(108, 110, 123, 134, 135, 145, 167, 187, 199)
p(108, 110, 123, 134, 135, 145, 167, 187, 199)


#Question 9
Q9 <- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Basic_Statistics_level_1\\Q9_a.csv")
view(Q9)
skewness(Q9$speed)
library("moments")
skewness(Q9$speed)
kurtosis(Q9$speed)

Q9b <- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Basic_Statistics_level_1\\Q9_b.csv")
skewness(Q9b$SP)
hist(Q9b$SP)
?boxplot()
boxplot(Q9b$SP,horizontal = TRUE)
kurtosis(Q9b$SP)
skewness(Q9b$WT)
kurtosis(Q9b$WT)

#Question10
## Get thi clear.

#Question11
# to calculate Z score
qnorm(0.97)#94%
qnorm(0.99)#98%
qnorm(0.98)#96%

## calulate manually the confidence interval
#94%=[115.48,284.52]
#98%=[95.81,304.19]
#96%=[107.88,292.12]


#Question12
data<-c(34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56)
mean(data)
median(data)
var(data)
sd(data)

#Question 20 Calculate probability from the given dataset for the below cases
cars<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Basic_Statistics_level_1\\Cars.csv")
MPG<-cars$MPG
View(MPG)
mean(MPG)
sd(MPG)
hist(MPG)

#Question 21 
wcat<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Basic_Statistics_level_1\\wc-at.csv")
hist(wcat$Waist)
hist(wcat$AT)

#Question 22 calculate z values
qnorm(0.95)
qnorm(0.97)
qnorm(0.80)

#Question 23 calculate t values
qt(0.975,24)
qt(0.98,24)
qt(0.995,24)

#Question 24
pt(-0.47,17)
