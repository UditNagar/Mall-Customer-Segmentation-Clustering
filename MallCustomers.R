#*****************************INSTALLING PACKAGES############################

#install.packages("ROCR")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("MASS")
#install.packages("mclust")
#install.packages("ggplot2")


#****************************CALLING LIBRARIES##############################

library(ROCR)
library(plyr)
library(dplyr)
library(arules)
library(arulesViz)
library(MASS)
library(mclust)
library(ggplot2)

#Our main aim is to identify the Mall Customer Segmentation patterns. By looking
#at the data we can assume that, how every customer needs to be specifically
#marketed to their different requirements. Companies can actually use this data
#to enhance their marketing techniques by understanding the preferences of every
#customer and use that to enhance their profits on the longer run

#To perform segmentation for this data we need to explore the data we have
#i.e. Mall Customers. By the looks of it, we have individual unique customer IDs
#, A categorical variable in the form of Gender and three columns of Age, 
#Annual Income and Spending Score which will be our main targets to identify
#the patterns in the customers' shopping and spending spree

#******************************CALLING CUSTOMER DATA****************************
MallCustomers <- read.csv("Mall_Customers.csv", header=TRUE)
attach(MallCustomers)
MallCustomers<-MallCustomers[complete.cases(MallCustomers),]
summary(MallCustomers)
str(MallCustomers)

#*******************VISUALISATION OF DIST OF MALES AND FEMALES****************
barplot(table(MallCustomers$Gender)
        ,main="Distribution of Males and Females"
        ,col=c("blue","red")
        ,xlab="Gender"
        ,ylab="count"
)


#****************************AGE ANALYSIS OF CUSTOMERS*************************

summary(MallCustomers$Age)

boxplot(MallCustomers$Age,
        col="red",
        main="Age Analysis of Customers")

#From the above boxplot, we can conclude that a large amount of ages are between
#30 and 35. Min Age is 18, Max Age is 70

#**************************ANNUAL INCOME ANALYSIS*****************************

summary(MallCustomers$Annual.Income..k..)

plot(density(MallCustomers$Annual.Income..k..),
        col="red",
        main="Annual Income Analysis of Customers",
     xlab="Annual Income",
     ylab="Density")
polygon(density(MallCustomers$Annual.Income..k..),
        col="green")

#Max and Min of Annual Income are 137 and 15 respectively with the mean being
#60.56 as we can see in the density plot as well, you can see the peak of the 
#distribution ranging in the region of 60-75

#It is also close to a normal distributtion but not a complete normal dist

#**************************SPENDING SCORE ANALYSIS*****************************


summary(MallCustomers$Spending.Score..1.100.)

hist(MallCustomers$Spending.Score..1.100.,
        col="red",
        main="Spending Score Analysis of Customers",
     xlab="Spending Score",
     ylab="Distribution of Frequency",
     label=TRUE)


#The min and max Spending Score are 1 and 99 respectively while as we can see
#from the histogram that the maximum number of spending scores lie between the
#range of 40 to 60

#******************************AGE vs ANNUAL INCOME****************************

ggplot(MallCustomers, aes(Age, Annual.Income..k..)) + 
  geom_point(stat = "identity", aes(color = as.factor(Gender))) + 
  labs(y = "Annual Income", x = "Age") +
  ggtitle("Age vs Annual Income") +
  scale_color_discrete(name="Gender")

#Until the age of 50, it is observed males have more income than females and
#after 50 it reverses

#******************************AGE vs SPENDING SCORE PLOT***********************


ggplot(MallCustomers, aes(Age, Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(Gender))) + 
  labs(y = "Spending Score", x = "Age") +
  ggtitle("Age vs Spending Score") +
  scale_color_discrete(name="Gender")

#After observing the plot we can say, Female shopping declines marginally after
#the age of 40. Females shop more than Males till 50
#Spending Score is inversely proportional to the Customer Age

#We now proceed to perform K-Means Clustering which will create different
#clusters to group customers with a similar spending activity based on their age
#and annual income. We first need to find the optimal number of clusters, we can
#find that with the help of a very common method known as the 'Elbow Method'

#*****************************K-MEANS CLUSTERING*******************************

#****************************TO REMOVE @JACK*********************************
#K-Means selects Random Values from the data and forms clusters we assign
#, takes the closest values from the centre of a particular cluster and reshape
#the plot (just like k-NN). The closest values are based on Euclidean Distance
#****************************TO REMOVE @JACK*********************************


ElbowEstimate<-vector(mode="numeric",length=10)
for (i in 1:length(ElbowEstimate)){
  KMeansData<-kmeans(MallCustomers[,c(4,5)],i)
  ElbowEstimate[i]<-KMeansData$tot.withinss 
}

#tot.withinss denotes the total intra-cluster sum of squares

ggplot(as.data.frame(ElbowEstimate),mapping=aes(y=ElbowEstimate,x=c(1:10))) +
       geom_point(col="red") +
       geom_line(col="blue") +
       ggtitle("Elbow Estimates") +
       xlab("Elbow Estimates") +
       ylab("K values")

#As we can see from the graph, K=5 bends the plot hence we can consider K=5
#for our analysis.
set.seed(300)
#Performing K-Means for K=5
KMCluster<-kmeans(MallCustomers[,c(4,5)],5)
#@JACK - Please show the results of the variable below in the document
KMCluster

MallCustomersCopy<-MallCustomers
MallCustomersCopy$ClusterNumber <- KMCluster$cluster

#The above copy of MallCustomers will tell which customer falls in what cluster
#after the K-Means Clustering Algorithm was implemented

#Plot

ggplot(MallCustomersCopy[,c(4,5)], aes(x = MallCustomersCopy$Annual.Income..k..
                            , y = MallCustomersCopy$Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(KMCluster$cluster))) +
  scale_color_discrete(name="Cluster Classification",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("C1", "C2", "C3", "C4", "C5")) +
  ggtitle("Clusters")+
  xlab("Annual Income")+ylab("Spending Score")


#C1 - Cluster 1: Customers with Low Annual Income and Low Spending Score
#C2 - Cluster 2: Customers with Low Annual Income and High Spending Score
#C3 - Cluster 3: Customers with High Annual Income and High Spending Score
#C4 - Cluster 4: Customers with High Annual Income and Low Spending Score
#C5 - Cluster 5: Customers with Medium Annual Income and Medium Spending Score

#Individual Analysis of all the Customers
MallCustomersCopy %>% group_by(ClusterNumber,Gender)%>% 
  summarise(med_age=median(Age),med_income = median(Annual.Income..k..), 
            med_spend = median(Spending.Score..1.100.))
#@JACK - Please show the above in a table in the document describing the results

#*****************************MODEL BASED CLUSTERING***************************

#*****************************MCLUST TO FIND BIC*******************************
MC<-Mclust(MallCustomers[,c(4,5)])

#@JACK add this summary in the document
summary(MC,parameters=TRUE)

#The BIC is -3633.453, the higher the BIC, the better the model fitting

#The best model has covariance matrices of EII which means equal volume and
#shape and orientation equal to the coordinate axis

#Plots
plot(MC,what="BIC")
plot(MC,what="classification")
plot(MC,what="uncertainty")
plot(MC,what="density")


#*****************************************************************************


