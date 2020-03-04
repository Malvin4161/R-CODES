##Set1
#Q1
Ques1<-c(24.23,25.53,25.41,24.14,29.62,28.25,25.81,24.39,40.26,32.95,91.36,25.99,39.42,26.71,35.00)
Qsort<- sort(Ques1, decreasing = FALSE)
View(Qsort)
mean(Qsort)
median(Qsort)
sd(Qsort)
var(Qsort)

##Mathematically
Qsort1=c(24.14,24.23,24.39,25.41,25.53,25.81,25.99);
Qsort3=c(28.25,29.62,32.95,35.00,39.42,40.26,91.36);
median(Qsort1)
median(Qsort3)

##R code
hist(Qsort)
boxplot(Qsort,horizontal=TRUE)
outliers<- boxplot.stats(Qsort)$out
View(outliers)


#Ques2 ,III
test<-c(0,1,2,2.5,3,4,5,6,6,6,6,6,9,9,9,12,12,12,12,16,17)
boxplot(test,horizontal = TRUE)
