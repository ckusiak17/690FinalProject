library(neuralnet)

train.size <- .8*nrow(dummy_data)
rows <- 1:nrow(dummy_data)
train.rows<-sample(rows, train.size, replace=F)
test.rows<-subset(rows, !rows%in%train.rows)
training.set <- dummy_data[train.rows,]
#training.set$spam <- NULL
test.set<- dummy_data[test.rows,]
test.set$spam<-NULL

#for repsonse
training.set.y <- spam[train.rows]
test.set.y <- spam[test.rows]
allVars <- colnames(training.set)
predictorVars <- allVars[!allVars%in%"spam"]
predictorVars <- paste(predictorVars, collapse ="+")
form=as.formula(paste("spam~",predictorVars,collapse="+"))

fit<-neuralnet(form, data=training.set, hidden = 10, threshold = 0.01,
          stepmax = 1e+05, rep = 1, startweights = NULL,
          learningrate.limit = NULL,
          learningrate.factor = list(minus = 0.5, plus = 1.2),
          learningrate=NULL, lifesign = "none",
          lifesign.step = 1000, algorithm = "rprop+",
          err.fct = "sse", act.fct = "logistic",
          linear.output = TRUE, exclude = NULL,
          constant.weights = NULL, likelihood = FALSE)

pred <- compute(fit, test.set)
predictions<-pred$net.result
library(pROC)
auc <- roc(test.set.y, predictions)
print(auc$auc)




detach(package:neuralnet,unload = T)

library(ROCR)
nn.pred = prediction(predictions, test.set.y)
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref)

