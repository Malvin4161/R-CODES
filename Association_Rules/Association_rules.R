## 1 book dataset
getwd()
setwd("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Association_Rules")
books<-read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Association_Rules\\book.csv")
View(books)  
str(books)

#EDA
books$ChildBks<- as.factor(books$ChildBks)
books$YouthBks<- as.factor(books$YouthBks)
books$CookBks<- as.factor(books$CookBks)
books$DoItYBks<- as.factor(books$DoItYBks)
books$RefBks<- as.factor(books$RefBks)
books$ArtBks<- as.factor(books$ArtBks)
books$GeogBks<- as.factor(books$GeogBks)
books$ItalCook<- as.factor(books$ItalCook)
books$ItalAtlas<- as.factor(books$ItalAtlas)
books$ItalArt<- as.factor(books$ItalArt)
books$Florence<- as.factor(books$Florence)


View(books)
str(books)

summary(books)

## Applying apriori function on books data frame 

rule= apriori(books)
arules::inspect(rule)
##rule.sorted<- sort(rule, by= "lift")
##arules::inspect(head(rule.sorted), 3)

inspect(head(sort(rule, by = "lift"), 3))  ## top 3 sorted by confidence


## Intall arulesviz library for plot vizualisation 
#install.packages("arulesViz")
library(arulesViz)

plot(rule, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL)

#creating rules with different support , min length confidence values
rule1 = apriori(books,parameter = list(minlen = 3,supp = 0.5,conf = 1)
                ,appearance = list(rhs = "ItalAtlas=0"))
arules::inspect(rule1)  ## When considering as rhs ItalAtlas=0,  number of rules = 65 which will lead to buy ItalAtlas

plot(rule1, method = NULL, measure = "support", shading = "lift", 
    interactive = FALSE, data = NULL, control = NULL)


rule2 = apriori(books,parameter = list(minlen = 1,supp = 0.3,conf = 0.6)
                ,appearance = list(rhs = "ItalCook=0"))
arules::inspect(rule2) ## When considering rhs italCook=0 number of rules = 865

plot(rule2, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL)


rule3 = apriori(books,parameter = list(minlen = 2,supp = 0.3,conf = 0.6)
                , appearance = list(rhs = "ItalCook=0"))
arules::inspect(rule3)  ## Considering min length of books in rules = 2 number of rules = 8817

plot(rule3, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL)

rule4= apriori(books,parameter = list(minlen = 4,supp = 0.5,conf = 0.9))
arules::inspect(rule4)

plot(rule4, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL, jitter = 0, engine='interactive')

#***************************************************************************              
## Groceries dataset
groceries<- read.csv("groceries.csv")
View(groceries)
str(groceries)
class(groceries)

attach(groceries)
data(groceries)
summary(groceries)


## applyig the apriori function 
ruleGro<- apriori(groceries)
arules::inspect(ruleGro)
ruleGro.Sorted<- sort(ruleGro, by="lift")
arules::inspect(ruleGro.Sorted) # Giving 5 rules


##plot(ruleGro, method = NULL, measure = "support", shading = "lift", 
    ## interactive = FALSE, data = NULL, control = NULL, jitter = 0)

plot(ruleGro,method="graph",interactive=TRUE,shading=NA, engine='interactive')

## CReating different rules with support and confidence changes
ruleGro1<- apriori(groceries, parameter = list(supp= 0.01, conf= 0.5))
arules::inspect(ruleGro1) # 96 rules

plot(ruleGro1, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL, jitter = 0)

ruleGro2<- apriori(groceries, parameter = list(minlen = 1, supp= 0.3, conf= 0.5), appearance = list(rhs="ready.soups="))
arules::inspect(ruleGro2)## Total rule count 1 for rules which leads to buy ready.soups 

plot(ruleGro2, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL, jitter = 0)

ruleGro3<- apriori(groceries, parameter = list(minlen = 3, supp= 0.2, conf= 0.4), appearance = list(lhs="ready.soups="))
arules::inspect(ruleGro3) ## 3rules  items together with 3485 count, 

plot(ruleGro3, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL, jitter = 0)

#**************************************************************
#3 my_movies dataset
movies<- read.csv("my_movies.csv")
View(movies)
str(movies)


movieRule<- apriori(as.matrix(movies[,6:15]),parameter = list(minlen = 1,supp = 0.2,conf = 0.5))
arules::inspect(movieRule)  ## Gives 15 rules

plot(movieRule,method= "graph",interactive=TRUE,shading=NA, engine='interactive')
## Above graph show how LOTR2 and LOTR1 purchases are interrelated and all other rules interaction

movieRule1<- apriori(as.matrix(movies[,6:15]), parameter = list(minlen = 1, supp = 0.2, conf= 0.5),
                                                                appearance = list(rhs="Patriot"))
arules::inspect(movieRule1)
## Above Rules  leads to buy Patriot movie and below graph show the pattern in which 
## people buying sixth sense and gladiator buys patriot movie too.

plot(movieRule1, method = "graph", interactive = TRUE, shading = NA, engine = 'interactive')

movieRule2<- apriori(as.matrix(movies[,6:15]), parameter = list(minlen= 2, supp = 0.5, conf=0.8),
                               appearance = list(rhs="Gladiator"))

arules::inspect(movieRule2)

## these rules shows the purchase of gladiator movie with supp= 0.5 and conf=0.8
## below graph shows the support and confidence measures leads to gladiator

plot(movieRule2, method = "graph", interactive = TRUE, shading = NA, engine = 'interactive' )
