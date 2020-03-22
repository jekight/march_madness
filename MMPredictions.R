library(tidyverse)
library(caret)
library(data.table)
library(Metrics)

#Load in Normalized Data
TourneyDiff <- read_csv("TourneyDiff.csv")

#Filter out the games that did not happen
RealGames <- TourneyDiff %>%
  filter(!is.na(TourneyDiff$Results))

#Training data will be from 2003 to 2018
Train1 <- RealGames %>%
  filter(Season <= 2018) %>%
  select(Results,POM_diff:PF_diff)

#Make Results a factor
Train1$Results <- as.factor(Train1$Results)

#Testing data will be 2019
Test1 <- RealGames %>%
  filter(Season == 2019) %>%
  select(Results,POM_diff:PF_diff)

#Make Results a factor
Test1$Results <- as.factor(Test1$Results)

control <- trainControl(method="repeatedcv", number=10, repeats = 5)
metric <- "Accuracy"

###################################################################################################

# kNN
set.seed(3142)
fit.knn <- train(Results~., data=Train1, method="knn",metric=metric, trControl=control)
# SVM
set.seed(3142)
fit.svm <- train(Results~., data=Train1, method="svmRadial",metric=metric, trControl=control)
# Random Forest
set.seed(3142)
fit.rf <- train(Results~., data=Train1, method="rf",metric=metric, trControl=control)
# Linear Algorithm
set.seed(3142)
fit.lda <- train(Results~., data=Train1, method="lda",metric=metric, trControl=control)
#Boosted Logistic Regression
set.seed(3142)
fit.blr <- train(Results~., data=Train1, method="LogitBoost",metric=metric, trControl=control)
#eXtreme Gradient Boosting Trees
set.seed(3142)
fit.xgbTree <- train(Results~., data=Train1, method="xgbTree",metric=metric, trControl=control)
#eXtreme Gradient Boosting Linear
set.seed(3142)
fit.xgbLM <- train(Results~., data=Train1, method="xgbLinear", metric=metric, trControl=control)
#Neural Network nnet
set.seed(3142)
fit.nnet <- train(Results~., data=Train1, method="nnet",metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, knn=fit.knn, svm=fit.svm, rf=fit.rf,
                          blr=fit.blr,xgbTree=fit.xgbTree,xgbLM=fit.xgbLM,nnet=fit.nnet))
summary(results)

#Predict 2019 outcomes using each model
predknn <- predict(fit.knn, newdata = Test1, type = "raw")
predsvm <- predict(fit.svm, newdata = Test1, type = "raw")
predrf <- predict(fit.rf, newdata = Test1, type = "raw")
predlda <- predict(fit.lda, newdata = Test1, type = "raw")
predblr <- predict(fit.blr, newdata = Test1, type = "raw")
predxgbTree <- predict(fit.xgbTree, newdata = Test1, type = "raw")
predxgbLM <- predict(fit.xgbLM, newdata = Test1, type = "raw")
prednnet <- predict(fit.nnet, newdata = Test1, type = "raw")

#Check the performance of each model
confusionMatrix(predknn, Test1$Results)
confusionMatrix(predsvm, Test1$Results)
confusionMatrix(predrf, Test1$Results)
confusionMatrix(predlda, Test1$Results)
confusionMatrix(predblr, Test1$Results)
confusionMatrix(predxgbTree, Test1$Results)
confusionMatrix(predxgbLM, Test1$Results)
confusionMatrix(prednnet, Test1$Results)

#####################################################################################################
# Determine Variable Importance
#####################################################################################################

Importance1 <- varImp(fit.rf)
plot(Importance1)

Importance2 <- varImp(fit.xgbTree)
plot(Importance2)

Importance3 <- varImp(fit.nnet)
plot(Importance3)

#####################################################################################################
# Take out some features
#####################################################################################################

Train2 <- RealGames %>%
  filter(Season <= 2018) %>%
  select(Results,POM_diff:Avg3pt_diff, OPP_SCORE_diff,
         BLK_diff:PF_diff)

#Make Results a factor
Train2$Results <- as.factor(Train2$Results)

# kNN
set.seed(3142)
fit2.knn <- train(Results~., data=Train2, method="knn", metric=metric, trControl=control)
# SVM
set.seed(3142)
fit2.svm <- train(Results~., data=Train2, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(3142)
fit2.rf <- train(Results~., data=Train2, method="rf", metric=metric, trControl=control)
# Linear Algorithm
set.seed(3142)
fit2.lda <- train(Results~., data=Train2, method="lda", metric=metric, trControl=control)
#Boosted Logistic Regression
set.seed(3142)
fit2.blr <- train(Results~., data=Train2, method="LogitBoost", metric=metric, trControl=control)
#eXtreme Gradient Boosting Trees
set.seed(3142)
fit2.xgbTree <- train(Results~., data=Train2, method="xgbTree", metric=metric, trControl=control)
#eXtreme Gradient Boosting Linear
set.seed(3142)
fit2.xgbLM <- train(Results~., data=Train2, method="xgbLinear", metric=metric, trControl=control)
#Neural Network nnet
set.seed(3142)
fit2.nnet <- train(Results~., data=Train2, method="nnet", metric=metric, trControl=control)

# summarize accuracy of models
results2 <- resamples(list(lda=fit2.lda, knn=fit2.knn, svm=fit2.svm, rf=fit2.rf,
                          blr=fit2.blr,xgbTree=fit2.xgbTree,xgbLM=fit2.xgbLM,nnet=fit2.nnet))
summary(results2)

#Predict 2019 using each model with limited features
predknn2 <- predict(fit2.knn, newdata = Test1, type = "raw")
predsvm2 <- predict(fit2.svm, newdata = Test1, type = "raw")
predrf2 <- predict(fit2.rf, newdata = Test1, type = "raw")
predlda2 <- predict(fit2.lda, newdata = Test1, type = "raw")
predblr2 <- predict(fit2.blr, newdata = Test1, type = "raw")
predxgbTree2 <- predict(fit2.xgbTree, newdata = Test1, type = "raw")
predxgbLM2 <- predict(fit2.xgbLM, newdata = Test1, type = "raw")
prednnet2 <- predict(fit2.nnet, newdata = Test1, type = "raw")

#Check the performance of each model
confusionMatrix(predknn2, Test1$Results)
confusionMatrix(predsvm2, Test1$Results)
confusionMatrix(predrf2, Test1$Results)
confusionMatrix(predlda2, Test1$Results)
confusionMatrix(predblr2, Test1$Results)
confusionMatrix(predxgbTree2, Test1$Results)
confusionMatrix(predxgbLM2, Test1$Results)
confusionMatrix(prednnet2, Test1$Results)

#Check variable impartance
Importance1b <- varImp(fit2.rf)
plot(Importance1b)

Importance2b <- varImp(fit2.xgbTree)
plot(Importance2b)

Importance3b <- varImp(fit2.nnet)
plot(Importance3b)

#####################################################################################################
# nnet Tuning
#####################################################################################################

print(fit.nnet)
print(fit2.nnet)

nnetgrid <- expand.grid(size = seq(from = 1, to = 5, by = 1),
                        decay = c(0.25,0.2,0.15, 0.1,0.075))

set.seed(3142)
nnet_tune1 <- train(Results~., 
                   data=Train1, 
                   method="nnet", 
                   metric=metric, 
                   trControl=control,
                   tuneGrid = nnetgrid)
print(nnet_tune1)

prednnet3 <- predict(nnet_tune1, newdata = Test1, type = "raw")
confusionMatrix(prednnet3, Test1$Results)

# Tune model with select freatures

set.seed(3142)
nnet_tune2 <- train(Results~., 
                    data=Train2, 
                    method="nnet", 
                    metric=metric, 
                    trControl=control,
                    tuneGrid = nnetgrid)
print(nnet_tune2)

prednnet4 <- predict(nnet_tune2, newdata = Test1, type = "raw")
confusionMatrix(prednnet4, Test1$Results)

#####################################################################################################
#NNET Logloss
#####################################################################################################

#Get the probalities for nnet_tune1
probnnet3 <- predict(nnet_tune1, newdata = Test1, type = "prob") %>%
  select(2)


EffectiveNNET <- RealGames %>%
  filter(Season == 2019) %>%
  mutate(Predictions = prednnet3) %>%
  select(ID,Results,Predictions)

EffectiveNNET <- cbind(EffectiveNNET,probnnet3) %>%
  rename(Probability = "1")

#Logloss for NNET model
EffectiveNNET <- EffectiveNNET %>%
  summarise(LogLoss = logLoss(Results,Probability),
            Errors = sum(((Probability < 0.0)+(Probability > 1.0)+(is.na(Probability)))))

print(EffectiveNNET)

#Get the probalities for nnet_tune1
probnnet4 <- predict(nnet_tune2, newdata = Test1, type = "prob") %>%
  select(2)


EffectiveNNET2 <- RealGames %>%
  filter(Season == 2019) %>%
  mutate(Predictions = prednnet4) %>%
  select(ID,Results,Predictions)

EffectiveNNET2 <- cbind(EffectiveNNET2,probnnet4) %>%
  rename(Probability = "1")

#Logloss for NNET model
EffectiveNNET2 <- EffectiveNNET2 %>%
  summarise(LogLoss = logLoss(Results,Probability),
            Errors = sum(((Probability < 0.0)+(Probability > 1.0)+(is.na(Probability)))))

print(EffectiveNNET2)

########################################################################################################
#Save Model
########################################################################################################

save(nnet_tune1, file = "nnet_model.rda")

load(file = "nnet_model.rda")
print(nnet_tune1)





