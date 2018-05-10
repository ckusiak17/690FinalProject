require(sparsediscrim)
require(mosaic)
require(xtable)
require(caret)
require(MASS)
require(kernlab)
require(glmnet)
require(pls)
require(randomForest)
library(pROC)
options(xtable.comment = FALSE)


source("preprocess.R")

#make an empty matrix to hold the prediction errors
#this is 7 X 10 for the 7 classification methods and the 10 CV folds
gbm_auc<- matrix(nrow = 1, ncol = 10)
method_names <- c("GBM")
gbm_prob <- matrix(nrow = 460, ncol = 10)

#change spambase here
set.seed(690)
spam <- mutate(dummy_data, spam = as.factor(spam), index = sample(rep(1:10, each = 460)))
for (i in 1:10){
  #create a training and test set
  
  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))
  
  
  
  #gbm
  train.set<-train
  y<- as.factor(ifelse(train.set$spam==0,'no','yes'))
  train.set$spam<-NULL
  
  gbm.fit <- train(train.set, #just a normal data frame
                   y, #has to be in yes or no format for some reason
                   method = "gbm",
                   metric = "ROC",
                   trControl = trainControl(method = "cv", number = 10, summaryFunction =  twoClassSummary, classProbs = TRUE),
                   preProcess = c("center", "scale")
  )
  
  

  gbm.prob<-predict(object = gbm.fit, test, type='prob')
  gbm.pred<-predict(object = gbm.fit, test, type='raw')
  gbm.pred<-ifelse(gbm.pred=='yes',1,0)
  
  gbm_prob[,i]<-gbm.prob[[2]]
  
  auc1 <- roc(test.spam, gbm.prob[[2]])
  gbm_auc[1,i]<-auc1$auc
  
}



rowMeans(gbm_auc)


#change the file name example: CrossValidationTemp2.Rda
save(gbm_auc, gbm_prob,
     file = "gbm_results.Rda")
  