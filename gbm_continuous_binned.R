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
load("spam.Rda")


source("preprocess.R")

#make an empty matrix to hold the prediction errors
#this is 7 X 10 for the 7 classification methods and the 10 CV folds
error <- auc <- false.pos <- matrix(nrow = 1, ncol = 10)
method_names <- c("GBM")
rownames(error) <- rownames(auc) <- rownames(false.pos) <- method_names


#change spambase here
set.seed(690)
spam <- mutate(temp, spam = as.factor(spam), index = sample(rep(1:10, each = 460)))
for (i in 1:10){
  #create a training and test set
  
  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))
  
  #univariate selection
  logistics <- lapply(subset(train, select = -spam), 
                      function(x) summary(glm(train$spam ~ x, family= "binomial"))$coefficients[2,4] )
  log.index <- as.vector(logistics < 0.05)
  train <- train[, log.index]
  test <- test[, log.index]
  
  
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
  
  
  auc1 <- roc(test.spam, gbm.prob[[2]])
  auc[1,i]<-auc1$auc
  
  error[1, i] <- mean(gbm.pred != test.spam)
  false.pos[1, i] <- table(gbm.pred, test.spam)[2,1]/
    (table(gbm.pred, test.spam)[2,1] + table(gbm.pred, test.spam)[1,1])
  
}


error <- cbind(error, rowMeans(error))
auc <- cbind(auc, rowMeans(auc))
false.pos <- cbind(false.pos, rowMeans(false.pos))

column.names <- c(paste0("sim", 1:10), "mean")
colnames(error) <- colnames(auc) <- colnames(false.pos) <- column.names

#change the file name example: CrossValidationTemp2.Rda
save(error, auc, false.pos,
     file = "gbmcontin_bin.Rda")
