library(gWidgets)
library(RMySQL)
library(DBI)
library(dbConnect)
library(igraph)
library(googleVis)
library(caret)
library(randomForest)

mydb=dbConnect(MySQL(),user='derek',password='derek1',dbname='7cot-fall15',host='130.108.85.104')
cdfnc <- dbSendQuery(mydb,'select numConversations from usersMembers')
numConv <- fetch(cdfnc,n=-1)
numConv <- numConv[complete.cases(numConv),]
nc.ecdf <- ecdf(numConv)
nc.ecdf
plot(nc.ecdf,xlab="Number of Conversations",ylab="Density",main="Cumulative Distribution",col="blue",pch=19,cex=0.9,xlim=c(0,50))
ns <- dbSendQuery(mydb,'select * from z_normalsample')
normal_sample <- fetch(ns,n=-1)
normal_sample <- normal_sample[complete.cases(normal_sample),]
row.names(normal_sample) <- NULL

normal_sample$Harasser <- as.factor("NO")
normal_sample$signupStrugglingWith <- as.factor(normal_sample$signupStrugglingWith)
normal_sample$updateEmail <- as.factor(normal_sample$updateEmail)
normal_sample$messageEmail <- as.factor(normal_sample$messageEmail)
normal_sample$feedFollowEmail <- as.factor(normal_sample$feedFollowEmail)
normal_sample$badgeGrowthEmail <- as.factor(normal_sample$badgeGrowthEmail)

unbiased_data <- rbind(normal_sample,harasser)
unbiased_data <- unbiased_data[sample(1:72690),] # randomly sample

tr <- unbiased_data[1:50833,] # 70% training data
te <- unbiased_data[50834:72690,] # 30% test data

unbiased_rf <- randomForest(Harasser~.,data=tr,ntree=1000,importance=T)
unbiased_rf
unbiased_p <- predict(unbiased_rf,te,type="response")

confusionMatrix(te$Harasser,unbiased_p)
table(te$Harasser)
varImpPlot(unbiased_rf,main="Random Forest Model") # After removing bias from normal users 

# Removing bias from harasser data
hs <- dbSendQuery(mydb,'select * from z_harsample')
harsample <- fetch(hs,n=-1)
harsample <- harsample[complete.cases(harsample),]
row.names(harsample) <- NULL

harsample$Harasser <- as.factor("YES")
harsample$signupStrugglingWith <- as.factor(harsample$signupStrugglingWith)
harsample$updateEmail <- as.factor(harsample$updateEmail)
harsample$messageEmail <- as.factor(harsample$messageEmail)
harsample$feedFollowEmail <- as.factor(harsample$feedFollowEmail)
harsample$badgeGrowthEmail <- as.factor(harsample$badgeGrowthEmail)

more_ub_data <- rbind(normal_sample,harsample)
more_ub_data <- more_ub_data[sample(1:68109),] # random sample
row.names(more_ub_data) <- NULL

# Training a Random Forest model on a normalized data
normalized_data <- scale(more_ub_data[,c(1,2,3,5,6,7,8,9,10,11,12)]) # normalizing data
normalized_data <- as.data.frame(normalized_data)
normalized_data <- cbind(normalized_data,more_ub_data[,c(4,13,14,15,16,17)])

normalized_tr <- normalized_data[1:47676,]
normalized_te <- normalized_data[47677:68109,]

n_rf <- randomForest(Harasser~.,data=normalized_tr,ntree=1000,importance=T)
n_rf 

n_p <- predict(n_rf,normalized_te,type="response")
confusionMatrix(normalized_te$Harasser,n_p)
table(normalized_te$Harasser)

varImpPlot(n_rf,main="Random Forest Model") # Removed bias from both normal users and harassers (Normalized)


ub_tr <- more_ub_data[1:47676,] # 70% training data
ub_te <- more_ub_data[47677:68109,] # 30% test data

ub_rf <- randomForest(Harasser~.,data=ub_tr,ntree=1000,importance=T)
ub_rf

ub_p <- predict(ub_rf,ub_te,type="response")
confusionMatrix(ub_te$Harasser,ub_p)
table(ub_te$Harasser)
varImpPlot(ub_rf,main="Random Forest Model") # Removed bias from both normal users and harassers


# T Test
norm_features <- c(109.4453,6.3358,483.6770,0.2935,8.2678,0.1599,11.6837,467.140,41.7608)
har_features <- c(70.95992,6.384641,430.0155,0.268609,15.32738,0.1929943,7.696194,501.6287,29.22179)
var.test(norm_features,har_features) # F test to compare variances (to ensure the homoskedasticity i.e. homogeneity of variances)
t.test(norm_features,har_features,var.equal=TRUE,paired=FALSE) # we can conclude that the averages of two groups are significantly similar
qt(0.975,15)

wilcox.test(norm_features,har_features,correct=FALSE) # Wilcox test when we cant assume the distribution to be normal

