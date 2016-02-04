library(gWidgets)
library(RMySQL)
library(DBI)
library(dbConnect)
library(igraph)
library(googleVis)
library(caret)
library(randomForest)

mydb=dbConnect(MySQL(),user='derek',password='derek1',dbname='7cot-fall15',host='130.108.85.104')

har <- dbSendQuery(mydb,'select * from z_hfeatures')
harasser <- fetch(har,n=-1)
harasser$Harasser <- as.factor("YES")
harasser$updateEmail <- as.factor(harasser$updateEmail)
harasser$messageEmail <- as.factor(harasser$messageEmail)
harasser$feedFollowEmail <- as.factor(harasser$feedFollowEmail)
harasser$badgeGrowthEmail <- as.factor(harasser$badgeGrowthEmail)
harasser$signupStrugglingWith <- as.factor(harasser$signupStrugglingWith)

harasser <- harasser[complete.cases(harasser),]
row.names(harasser) <- NULL

normal <- dbSendQuery(mydb,'select signupDateU,lastLoginU,signupDistressLevel,signupStrugglingWith,growthPoints,forumUpvotes,numConversations,offlineMsgCount,compassionHearts,currentLevel,coins,groupSupportMsgs,updateEmail,messageEmail,feedFollowEmail,badgeGrowthEmail from usersMembers where memID not in (select bannerID from z_h) limit 400000')
normal_users <- fetch(normal,n=-1)
 
normal_users$Harasser <- as.factor("NO")
normal_users$updateEmail <- as.factor(normal_users$updateEmail)
normal_users$messageEmail <- as.factor(normal_users$messageEmail)
normal_users$feedFollowEmail <- as.factor(normal_users$feedFollowEmail)
normal_users$badgeGrowthEmail <- as.factor(normal_users$badgeGrowthEmail)
normal_users$signupStrugglingWith <- as.factor(normal_users$signupStrugglingWith)

normal_users <- normal_users[complete.cases(normal_users),]

data_ml <- rbind(normal_users,harasser)
data_ml <- data_ml[sample(1:271785),] # randomly sample

trainset <- data_ml[1:190250,] # 70% training data 
testset <- data_ml[190251:271785,] # 30% test data

library(randomForest)
rf <- randomForest(Harasser~.,data=trainset,ntree=1000,importance=T)
rf
p <- predict(rf,testset,type="response")
library(caret)
confusionMatrix(testset$Harasser,p)
table(testset$Harasser)

par(mar=c(5.1,4.1,4.1,2.1))
varImpPlot(rf,main="Random Forest Model")


normal_6000 <- normal_users[1:5938,] # trying to make dataset more balanced for the random forest model
data_ml_6000 <- rbind(normal_6000,harasser)
data_ml_6000 <- data_ml_6000[sample(1:11876),] 
row.names(data_ml_6000) <- NULL

trainset_6000 <- data_ml_6000[1:8300,] # 70% training data
testset_6000 <- data_ml_6000[8301:11876,] # 30% test data

rf_6000 <- randomForest(Harasser~.,data=trainset_6000,ntree=1000,importance=T)
rf_6000
p_6000 <- predict(rf_6000,testset_6000,type="response")
confusionMatrix(testset_6000$Harasser,p_6000)
table(testset_6000$Harasser) 

varImpPlot(rf_6000,main="Random Forest Model") # Balanced Data 

