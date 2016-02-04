complete_normal$Harasser <- "NO"
complete_normal$Harasser <- as.factor(complete_normal$Harasser) #  convert the harasser variable into factor 

harasser$Harasser <- "YES"
harasser$Harasser <- as.factor(harasser$Harasser) # convert the harasser variable into factor 

row.names(complete_normal) <- NULL # remove row names
row.names(harasser) <- NULL

head(harasser)

harasser_ml <- harasser[,-9] # remove the bannerID variable for random forest model
head(harasser_ml)

complete_normal_ml <- complete_normal[,-1] # remove the memID variable for random forest model
head(complete_normal_ml)

data_ml <- rbind(complete_normal_ml,harasser_ml)
data_ml <- data_ml[sample(1:271785),] # randomly sample

trainset <- data_ml[1:190250,] # 70% training data 
testset <- data_ml[190251:271785,] # 30% test data

library(randomForest)
rf <- randomForest(Harasser~.,data=trainset,ntree=1000)
rf
p <- predict(rf,testset,type="response")
pdf("VarImpPlot_unbalanced.pdf")
varImpPlot(rf,main="Random Forest Model")
dev.off()

library(caret)
confusionMatrix(testset$Harasser,p) # performs poorly 0 Specificity (maybe because the dataset is highly unbalanced)
table(testset$Harasser)

normal_6000 <- complete_normal_ml[1:5938,] # trying to make dataset more balanced for the random forest model
data_ml_6000 <- rbind(normal_6000,harasser_ml)
data_ml_6000 <- data_ml_6000[sample(1:11876),] 
row.names(data_ml_6000) <- NULL

trainset_6000 <- data_ml_6000[1:8300,] # 70% training data
testset_6000 <- data_ml_6000[8301:11876,] # 30% test data

rf_6000 <- randomForest(Harasser~.,data=trainset_6000,ntree=1000)
rf_6000
p_6000 <- predict(rf_6000,testset_6000,type="response")
confusionMatrix(testset_6000$Harasser,p_6000) # Performs extraordinary with 97% sensititvity 100% Specificity 98% overall accuracy
pdf("VarImpPlot_balanced.pdf")
varImpPlot(rf_6000,main="Random Forest Model")
dev.off()
table(testset_6000$Harasser) 

# Plotting ROC
library(ROCR)
test.forest <- predict(rf,type="prob",newdata=testset)
forestpred <- prediction(test.forest[,2],testset$Harasser)
forestperf <- performance(forestpred,"tpr","fpr")
plot(forestperf,main="ROC",col="blue")

test.forest_6000 <- predict(rf_6000,type="prob",newdata=testset_6000)
forestpred_6000 <- prediction(test.forest_6000[,2],testset_6000$Harasser)
forestperf_6000 <- performance(forestpred_6000,"tpr","fpr")
plot(forestperf_6000,main="ROC",col="blue")

