##Question 1.A F&B manager wants to determine whether there is any significant difference in the diameter of the cutlet between two units. A randomly selected sample of cutlets was collected from both units and measured? Analyze the data and draw inferences at 5% significance level. Please state the assumptions and tests that you carried out to check validity of the assumptions.A F&B manager wants to determine whether there is any significant difference in the diameter of the cutlet between two units. A randomly selected sample of cutlets was collected from both units and measured? Analyze the data and draw inferences at 5% significance level. Please state the assumptions and tests that you carried out to check validity of the assumptions.
## Using Cutlets.mtw
cutlets<-read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Hypothesis_Testing\\Cutlets.csv")
attach(cutlets)


## Normality Test

shapiro.test(Unit.A)##P >0.05 Data normal
shapiro.test(Unit.B)##P >0.05 Data normal


## Variance test
var.test(Unit.A,Unit.B) ##P>0.05 ,so p high null fly => Equal variances


## 2 sample t test
t.test(Unit.A,Unit.B, alternative = "two.sided",conf.level = 0.95,correct = TRUE)
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.4723 > 0.05 accept null Hypothesis 
# unequal means

##**************************************************************

##Question 2 A hospital wants to determine whether there is any difference in the average Turn Around Time (TAT) of reports of the laboratories on their preferred list. They collected a random sample and recorded TAT for reports of 4 laboratories. TAT is defined as sample collected to report dispatch.
LabTAT<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Hypothesis_Testing\\LabTAT.csv")
Stacked_Data <- stack(LabTAT)
attach(Stacked_Data)


##Normality test
shapiro.test(LabTAT$Laboratory.1)##P 0.5508>0.05 data normal
shapiro.test(LabTAT$Laboratory.2)##P 0.08637>0.05 data normal
shapiro.test(LabTAT$Laboratory.3)##P 0.4205>0.05 data normal
shapiro.test(LabTAT$Laboratory.4)##P 0.6619>0.05 data normal

## ANOVA test
library(car)
?car
leveneTest(values~ ind, data = Stacked_Data)## P 0.05161>0.05 Equal variance
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value = 2e-16 < 0.05 accept alternate hypothesis 
# All Proportions not equal 

##***********************************************************************
##Ques 3 Sales of products in four different regions is tabulated for males and females. Find if male-female buyer rations are similar across regions.
Buyers<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Hypothesis_Testing\\BuyerRatio.csv")
Buyers2<-Buyers[,-1]
attach(Buyers)
chisq.test(Buyers2)

## P >0.05 , so fail to reject null hypothesis, All male female buyers ratio are similar.




##****************************************************************************
##Ques 4 TeleCall uses 4 centers around the globe to process customer order forms. They audit a certain %  of the customer order forms. Any error in order form renders it defective and has to be reworked before processing.  The manager wants to check whether the defective %  varies by centre. Please analyze the data at 5% significance level and help the manager draw appropriate inferences
Orders<-read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Hypothesis_Testing\\Costomer+OrderForm.csv")
attach(Orders)

?chisq.test




#*************************************************************************
#Ques 5 Fantaloons Sales managers commented that % of males versus females walking in to the store differ based on day of the week. Analyze the data and determine whether there is evidence at 5 % significance level to support this hypothesis.
fanta<-read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Hypothesis_Testing\\Faltoons.csv")
attach(fanta)
table1 <- table(Weekdays,Weekend)
table1
prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

## P>0.05 accept null hypothesis
##i.e equal proportions

prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "less")
