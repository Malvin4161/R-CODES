##1 Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters.
getwd()
setwd("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Clustering_Assignment")


library("readxl")
airlines<- read_xlsx("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Clustering_Assignment\\EastWestAirlines.xlsx", sheet = 2)
View(airlines)


attach(airlines)

##EDA

plot(airlines$`ID#`, airlines$Balance)
plot(airlines$`ID#`, airlines$Qual_miles)
plot(airlines$`ID#`, airlines$cc1_miles )
plot(airlines$`ID#`, airlines$Bonus_miles)
plot(airlines$`ID#`, airlines$Bonus_trans)
plot(airlines$`ID#`, airlines$Flight_miles_12mo)
plot(airlines$`ID#`,airlines$Days_since_enroll)

library(caret)
preproc<- preProcess(airlines)
summary(preproc)
summary(airlines)
str(airlines)


## No missing or null values so no imputation required.

##creating the normalize function
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

## normalizing the airlins data
normalized_data<-as.data.frame(lapply(airlines[,2:12],normalize))
summary(normalized_data)


## Or can apply standardize
normalized_data <- scale(airlines[,2:12]) #excluding the university name columnbefore normalizing
summary(normalized_data)

## HIERARCHICAL process to get clusters

d<- dist(normalized_data, method = "euclidean")
fit<- hclust(d, method = "centroid")

plot(fit)


groups<- cutree(fit, k=5)
rect.hclust(fit, k=5,border = "red")


clusters1<-as.matrix(groups)
final<- data.frame(airlines, clusters1)
View(final)


clusters<-aggregate(airlines, by=list(final$clusters1), FUN=mean)
summary(clusters)
View(clusters)


# since the above hierarchical approach is not suitble for large data sets, Try K- means

# k mean  process

kfit<- kmeans(normalized_data,4)
str(kfit)
table1<-data.frame(airlines, kfit$cluster)
table2<- table1[,c(ncol(table1),1:(ncol(table1)-1))]
View(table2)

aggregate(airlines[,-c(1,2)], by=list(kfit$cluster), FUN=mean)
# kmeans by screee plot
?apply
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))
wss
twss = c()
for (i in 1:8) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:8, twss, type="b", xlab="Number of Clusters", ylab="total Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

## by the scree plot we can say we can divide data into 5 clusters which will give the best result

#INFERENCES
# Group5 received highest Awards(free flights) also have highest Bonus_miles and have travlled the highest in last 12 mo
# Group1 and group 2 has lowest number of awards as they have the lowest flight miles values in 12 months.
#




##***********************************************************************************
##2 Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

crime_data<- read.csv("F:\\Study\\DATA_SCIENCE\\Assignment_Mtrl\\Clustering_Assignment\\crime_data.csv")
View(crime_data)
str(crime_data)
summary(crime_data)

attach(crime_data)

library(car)
plot(X, Murder)
plot(X, Assault)
plot(X, Rape)
plot(X, UrbanPop)


normalized_Crime_data<- as.data.frame(lapply(crime_data[,2:5], normalize))
summary(normalized_Crime_data)


## Hierarchical process
dcrime<- dist(normalized_Crime_data, method = "euclidean")
fit<- hclust(dcrime,method = "complete")
plot(fit)
plot(fit,hang = 1)
clusterCrime1<- cutree(fit, k=4)

rect.hclust(fit, k=4, border="green")


clusterGroup<- as.matrix(clusterCrime1)
crime_data1<- data.frame(crime_data,clusterGroup)
View(crime_data1)

crime_data2<- crime_data1[c(6,1,2,3,4,5)]  ## rearranging columns
View(crime_data2)

aggregate(crime_data[,2:5], by=list(crime_data2$clusterGroup), mean)


## k means using non hierarchical process
twss=c()
for (i in 1:8) twss[i] = sum(kmeans(normalized_Crime_data, centers=i)$withinss)
plot(1:8, twss, type="b", xlab="Number of Clusters", ylab="total Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")


# INFERENCES

# Above crime data cab se divided into 4 clusters which has a different set values 
# for murder, assault, urbanPop and Rape
# using the cluter group and X value we can determine which state has higher crime rate
# Alabama, Alaska etc comes in cluster 1 group which has higher Murder and Rape cases.
# Group2 2 has higher Assaults which has higher UrbanPop.Example Arizona.
# Group 4 states has least crime rate example: Idaho
