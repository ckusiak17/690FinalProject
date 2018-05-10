library(neuralnet)


#container for output of this loop
nn_prob <- matrix(nrow = 460, ncol = 10)
nn_auc <- matrix(nrow = 1, ncol = 10)

#for some reason this neural net needs response to be numeric instead of factor
#change spambase here
set.seed(690)
spam <- mutate(dummy_data, index = sample(rep(1:10, each = 460)))
for (i in 1:10){
  #create a training and test set
  
  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))
  
  
  #formula
  allVars <- colnames(train)
  predictorVars <- allVars[!allVars%in%"spam"]
  predictorVars <- paste(predictorVars, collapse ="+")
  form=as.formula(paste("spam~",predictorVars,collapse="+"))
  
  fit<-neuralnet(form, data=train, hidden = 10, threshold = 0.01,
            stepmax = 1e+05, rep = 1, startweights = NULL,
            learningrate.limit = NULL,
            learningrate.factor = list(minus = 0.5, plus = 1.2),
            learningrate=.1, lifesign = "none",
            lifesign.step = 1000, algorithm = "rprop+",
            err.fct = "sse", act.fct = "logistic",
            linear.output = TRUE, exclude = NULL,
            constant.weights = NULL, likelihood = FALSE)
  
  pred <- compute(fit, test)
  predictions<-pred$net.result
  nn_prob[,i]<-predictions
  nn_auc[,i] <- roc(test.spam, predictions)$auc

  print(paste("Fold number", i, "is now finished.", sep = " "))
  
}

  
  
  
detach(package:neuralnet,unload = T)
  
library(ROCR)
nn.pred = prediction(predictions, test.set.y)
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref)

