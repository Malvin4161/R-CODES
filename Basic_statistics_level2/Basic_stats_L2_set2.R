## Basic stats level 2 set2 
#Qu 1.	The time required for servicing transmissions is normally distributed with mu = 45 minutes and sigma = 8 minutes. The service manager plans to have work begin on the transmission of a customer's car 10 minutes after the car is dropped off and the customer is told that the car will be ready within 1 hour from drop-off. What is the probability that the service manager cannot meet his commitment? 
1-pnorm(50,45,8)


## Qu 2. 2.	The current age (in years) of 400 clerical employees at an insurance claims processing center is normally distributed with mean mu = 38 and Standard deviation sigma =6. For each statement below, please specify True/False. If false, briefly explain why.
#A.	More employees at the processing center are older than 44 than between 38 and 44.
1-pnorm(44,38,6)   ## 84.13%
pnorm(38,38,6)  ## 50%

pnorm(44,38,6)-pnorm(38,38,6)

## Qu 3. 	If X1 ~ N(??, ??2) and X2 ~ N(??, ??2) are iid normal random variables, then what is the difference between 2 X1 and X1 + X2? Discuss both their distributions and parameters
## ?????????

## Qu 4.4.	Let X ~ N(100, 202). Find two values, a and b, symmetric about the mean, such that the probability of the random variable taking a value between them is 0.99. 
