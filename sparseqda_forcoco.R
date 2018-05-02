

#### read from here down

training.set<-train
test.spam 
test.set <- test


#model
fit<-dqda(spam~.,data=training.set)
pred<-predict(fit, newdata=test.set)
prediction<-pred$posterior[,1]
category<-test.spam
#library(pROC) #ignore this because you use glmnet
#roc_obj <- roc(category, prediction)
#auc(roc_obj)



#Wow! dummy set has better performance
#quickly make a training and a test set 80/20 just because thats easy
#for predictors
train.size <- .8*nrow(dummy_data)
rows <- 1:nrow(dummy_data)
train.rows<-sample(rows, train.size, replace=F)
test.rows<-subset(rows, !rows%in%train.rows)
training.set <- dummy_data[train.rows,]
test.set<- dummy_data[test.rows,]
test.set$spam<-NULL
#for repsonse
training.set.y <- spam[train.rows]
test.set.y <- spam[test.rows]

#model
fit<-dqda(spam~.,data=training.set)
pred<-predict(fit, newdata=test.set)
prediction<-pred$posterior[,1]
category<-test.set.y
library(pROC)
roc_obj <- roc(category, prediction)
auc(roc_obj)