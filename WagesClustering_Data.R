library(plyr)
library(ggplot2)

#############read file################
Wages_data<-read.csv("D:/02 MSC FE/Sem 2/Computational finance/Project/Guide to modern econometrics/WagesClustering.csv")
str(Wages_data)
#Scatter plot#####
attach(Wages_data)
Wages_data[,2]
plot(Wages_data[,2],Wages_data[,4],main="Wages",xlab="exper",ylab="wages",pch=19)
View(Wages_data)
####Normalization####
z<-Wages_data[,-c(1,1)]
z
m<-apply(z,2,mean)
s<-apply(z,2,sd)
z<-scale(z,m,s)
##########Calculating Euclidean distance#############
distance<-dist(z)
#distance
#print(distance,digits=3)
#View(distance)

#####Cluster dendogram complete linkage####
hc.c<-hclust(distance)
plot(hc.c)

#########Cluster dendogram average linkage#####
hc.a<-hclust(distance,method="average")
plot(hc.a,hang=-1)

###############k means clustering###############
wage<-Wages_data[,-c(1,1)]
wage
k<-kmeans(wage,30)
k
table(Wages_data$"sex",k$cluster)
plot(Wages_data[c("school","wage")],k$cluster)
plot(Wages_data,col=k$cluster)
points(k$center,col=1:2,pch=8,cex=1)
wss <- (nrow(wage)-1)*sum(apply(wage,2,var))for (i in 2:3295) wss[i] <- sum(kmeans(mydata,wage,centers=i)$withinss)
warnings()
fit<-kmeans(wage,30)
fit
aggregate(wage,by=list(fit$cluster),FUN=mean)
wage<-data.frame(wage,fit$cluster)
wage

d<-dist(wage,method="euclidean")
#d
fit<-hclust(d,method="ward.D2")
plot(fit)
groups<-cutree(fit,k=30)
rect.hclust(fit,k=30,border="red")
fit<-kmeans(wage,30)
library(cluster)

wssplot <- function(wage, nc=165, seed=1234){
  wss <- (nrow(wage)-1)*sum(apply(wage,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(wage, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wage, nc=165) 

clusplot(wage,k$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

