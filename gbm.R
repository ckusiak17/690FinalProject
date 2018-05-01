# Uses same train/test as above
# This script runs both sets of data, log scaled and dumy encoded
# for continuous
train.size <- .8*nrow(temp2)
rows <- 1:nrow(temp2)
train.rows<-sample(rows, train.size, replace=F)
test.rows<-subset(rows, !rows%in%train.rows)
training.set <- temp2[train.rows,]
training.set$spam <- NULL
test.set<- temp2[test.rows,]
test.set$spam<-NULL
#for repsonse

training.set.y <- spam[train.rows]
test.set.y <- spam[test.rows]




y<- as.factor(ifelse(training.set.y==0,'no','yes'))
levels(y)



model_test <- train(training.set, #just a normal data frame
                    y, #has to be in yes or no format for some reason
                    method = "gbm",
                    metric = "ROC",
                    trControl = trainControl(method = "cv", number = 10, summaryFunction =  twoClassSummary, classProbs = TRUE),
                    preProcess = c("center", "scale")
)


summary(model_test)
# Plot model
plot(model_test)

predictions<-predict(object = model_test, test.set, type='prob')
auc <- roc(test.set.y, predictions[[2]])
print(auc$auc)


