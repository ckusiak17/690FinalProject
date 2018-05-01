install.packages('sparsediscrim', dependencies = TRUE)
library(sparsediscrim)
#############################################################
###############         SPARSE QDA          #################
############################################################
# This script runs both sets of data, log scaled and dumy encoded
# for continuous
train.size <- .8*nrow(temp2)
rows <- 1:nrow(temp2)
train.rows<-sample(rows, train.size, replace=F)
test.rows<-subset(rows, !rows%in%train.rows)
training.set <- temp2[train.rows,]
test.set<- temp2[test.rows,]
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