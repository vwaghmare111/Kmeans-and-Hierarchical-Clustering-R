library(ggplot2)

#importing the data###
setwd("D:/02 MSC FE/Sem 2/Computational finance/Project")
mydata<-read.csv("Wages1.csv")
#checking the data imported
View(mydata)
head(mydata)
class(mydata)
names(mydata)
#checking the class of the variables
sapply(mydata,class)
##Converting categorical data to binary####
#install.packages("plyr")
library(plyr)
mydata$sex<-revalue(mydata$sex,c("male"="1","female"="0"))
head(mydata)
tail(mydata)

#checking for outliers
attach(mydata)
plot(mydata[,4],xlab="Wages",main="Wages")
quantile(mydata[,4],c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,.9995,1))
#capping the outliers
mydata$Capped_wages<-ifelse(mydata[,4]>31.44022444,31.44022444,mydata[,4])
names(mydata)
#removing the orignal wages
mydata1<-mydata[,-4]
names(mydata1)
View(mydata)
plot(mydata$Capped_wages,xlab="Capped Wages",main="Capped Wages")
#checking relation
#install.packages("ggplot2")
library(ggplot2)
qplot(Capped_wages,exper,data=mydata1,color=exper,alpha=1,color=Capped_wages,main="Scatter plot Wages vs Experience",xlab="Wages",ylab="Experience")
qplot(Capped_wages,school,data=mydata1,color=school,alpha=1,color=Capped_wages,main="Scatter plot Wages vs Schooling years",xlab="Wages",ylab="Schooling years")
cor<-cor(mydata1[,c(-2)])
#############regression between gender and wages################
genwages<-lm(Capped_wages~sex,data=mydata1)
summary(genwages)
male<-mydata1[1570:3294,]
females<-mydata1[1:1569,]
mean(male$Capped_wages)
mean(females$Capped_wages)
tstat<-1.16192/0.11134
tstat
########Confidence interval##############
Ubound<-1.17+1.96*0.11
Ubound
Lbound<-1.17-1.96*0.11
Lbound

#dividing the data set into training and validation
sample<-sample(2,nrow(mydata1),replace = TRUE,prob = c(.8,.2))
tdata<-mydata1[sample==1,]
vdata<-mydata1[sample==2,]

#predicting on tdata####
model2<-lm(Capped_wages~exper+sex+school,data = tdata)
summary(model2)
pred<-predict(model2,vdata)
head(pred)
predict<-as.data.frame(pred)
error<-predict-mydata1$Capped_wages
errorsq<-error^2
summary(errorsq)


#fitting the complete model##
model<-lm(Capped_wages~exper+sex+school,data = mydata1)
summary(model)
anova(model)

#####################Hierarchical Clustering######################
d<-dist(mydata1,method="euclidean")
H.fit<-hclust(d,method="ward.D2")
plot(H.fit)
groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red")
table(mydata1[,1],groups)
#barplot(table(hc[,1],groups), 
        #xlab="Number of Clusters", ylab="Number of Criteria",
        #main="Number of Clusters Chosen")
####################Kmeans Clustering#############################
set.seed(123456789)
schwages<-kmeans(mydata1[,c("school","Capped_wages")],centers=3,nstart=10)
schwages
plot(mydata1$school, mydata1$Capped_wages, type="n", xlim=c(3,19), xlab="Wage", ylab="School")
text(x=mydata1$Capped_wages, y=mydata1$school, labels=mydata1$sex,col=schwages$cluster+1)
expwages<-kmeans(mydata1[,c("exper","Capped_wages")],centers=3,nstart=10)
expwages
plot(mydata1$exper, mydata1$Capped_wages, type="n", xlim=c(3,19), xlab="Wage", ylab="Experience")
text(x=mydata1$Capped_wages, y=mydata1$exper, labels=mydata1$sex,col=schwages$cluster+1)
library(cluster)
clusplot(mydata1[,-1], schwages$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(mydata1[,-1], expwages$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)



