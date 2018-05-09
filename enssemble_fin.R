#install.packages('sparsediscrim', dependencies = TRUE)
require(mosaic)
require(xtable)
require(caret)
require(MASS)
require(kernlab)
require(glmnet)
require(pls)
require(sparsediscrim)
require(randomForest)
require(pROC)
library(data.table)
require(parallelSVM)
require(foreach)
require(parallel)
options(xtable.comment = FALSE)
#load("spam.Rda")
source("preprocess.R")



#Split your data into folds
set.seed(690)
cvfold <- 10
fold_size <- nrow(dummy_data)/cvfold
spam <- mutate(dummy_data, spam = as.factor(spam), index = sample(rep(1:cvfold, each = fold_size)))
#name your repsonse response! lets make this reproducible
#future work



#make an empty matrix to hold the prediction errors
#this is 7 X 10 for the 7 classification methods and the 10 CV folds
method_names <- c( "LASSO", "KNN", "PCR", "RandomForest", "LDA", "SVMpolynomial", "SVMlinear")
method_probs <- array(dim=c(length(method_names), cvfold, fold_size))
error <- auc <- false.pos <- matrix(nrow = length(method_names), ncol = cvfold)
rownames(error) <- rownames(auc) <- rownames(false.pos) <- method_names

lambda.l <- k.knn <- ncomp.pcr <- nodes.rf <- c()
  plog.l <- knn <- pcr <- 
  svm.l <- list()

which.lasso <- which.ridge <- which.elastic <- c()

for (i in 1:cvfold){
  
  
  #create a training and test set
  
  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))
  
  
  
  
  
  #LASSO
  mname = 'LASSO'
  method_index <- which(method_names%in%mname)
  #data set up for penalized logistic regressions
  x <- as.matrix(subset(train, select = -spam)) #removes spam from the dataframe
  x.test <- as.matrix(test)
  
  #lasso penalty, alpda = 1
  plog.l.fit <- glmnet(x, train$spam, family = "binomial", alpha=1, standardize = TRUE)
  plog.l.prob <- predict(plog.l.fit, x.test, type = "response")
  plog.l.pred <- ifelse(plog.l.prob > .5, 1, 0)
  l.error <- c()
  l.false <- c()
  l.auc <- c()
  for (lambda in 1:ncol(plog.l.pred)){ #length(plog.l.fit$lambda) might be more intuitive
    l.error[lambda] <-  mean(plog.l.pred[,lambda] != test.spam) # can we use auc instead of error for lambda?
    auc1 <- roc(test.spam, plog.l.prob[,lambda]) #lets use auc instead of error for choosing lambda
    l.auc[lambda] <- auc1$auc
    l <- data.frame(pred = plog.l.pred[,lambda], true = test.spam)
    l.false[lambda] <- nrow(filter(l, pred == 1, true == 0))/nrow(filter(l, true == 0))
  }
  error[method_index, i] <- min(l.error) #chooses error for best penalty
  false.pos[method_index, i] <- l.false[which.min(l.error)]
  #which.lasso[i] <- which.max(l.auc)#chooses lambda
  auc1 <- roc(test.spam,plog.l.prob[,which.max(l.auc)])
  auc[method_index, i] <- auc1$auc
  #lambda.l[i] <- plog.l.fit$lambda[which.max(l.auc)] #identifies best lambda from model fit
  method_probs[method_index,i,]<-plog.l.prob[,which.max(l.auc)]
  
  
  ##################################################################################################################################
  ##################################################################################################################################
  
  
  
  #K-Nearest Neighbors
  #this runs a 10 fold cross validation to determine
  #which value of k is optimal for knn
  mname <- "KNN"
  method_index <- which(method_names%in%mname)
  
  trControl <- trainControl(method  = "cv",
                            number  = 10)
  knn.fit <- train(spam ~ .,
                   method     = "knn",
                   tuneGrid   = expand.grid(k = 1:10),
                   trControl  = trControl,
                   metric     = "Accuracy",
                   data       = train)

  knn.pred <- predict(knn.fit, test)
  knn.probability <- predict(knn.fit, test, type="prob")
  method_probs[method_index,i,]<-knn.probability[[2]]
  error[method_index, i] <- mean(knn.pred != test.spam)  
  auc1 <- roc(test.spam, knn.probability[[2]])
  auc[method_index, i] <- auc1$auc
  false.pos[method_index, i] <- table(knn.pred, test.spam)[2,1]/(table(knn.pred, test.spam)[2,1] + table(knn.pred, test.spam)[1,1])

  
  ##################################################################################################################################
  ##################################################################################################################################
  
  
  
  #Principle Components Regression
  mname="PCR"
  method_index <- which(method_names%in%mname)
  
  nofactor.spam <- mutate(spam, spam = ifelse(spam == 1, 1, 0))
  nofactor.train <- subset(filter(nofactor.spam, index != i), select = -index)
  nofactor.test.spam <- filter(nofactor.spam, index == i)$spam
  nofactor.test <- subset(filter(nofactor.spam, index == i), select = - c(index, spam))
  train.matrix <- model.matrix(spam ~. , data = nofactor.train)[,-1]
  train.spam <- nofactor.train$spam
  pcr.fit <- pcr(spam ~., data = nofactor.train, scale = TRUE, validation = "CV")
  aa1 <- pcr.fit[["validation"]][["PRESS"]]
  ncomp.pcr[i] <- which(aa1==min(aa1))
  pcr.prob <- predict(pcr.fit, nofactor.test,  ncomp = which(aa1==min(aa1)))
  method_probs[method_index, i, ] <- pcr.prob
  pcr.pred <- ifelse(pcr.prob > .5, 1, 0)
  error[method_index, i] <- mean(pcr.pred != test.spam)
  auc1 <- roc(test.spam, pcr.prob)
  auc[method_index, i] <- auc1$auc
 
  
  ##################################################################################################################################
  ##################################################################################################################################
  
  
  #Random Forests
  mnames<-"RandomForest"
  method_index<-which(method_names%in%mnames)
  

  # Random Search
  training.set <- train
  training.set.y <- training.set$spam
  training.set$spam <- NULL
  test.set.y<-test.spam
  
  y<- as.factor(ifelse(training.set.y==0,'no','yes'))
  training.set$spam <- y
  
  control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid", classProbs = TRUE, summaryFunction = twoClassSummary)
  tunegrid <- expand.grid(.mtry=c(1:15))
  mtry <- sqrt(ncol(x))
  metric <- "ROC"
  rf_random <- train(spam~., data=training.set, method="rf", metric=metric, tuneGrid = tunegrid, trControl=control)
  #decision.values = TRUE
  #test1 <- test
  #test1$spam <- as.factor(ifelse(test.spam==0,'no','yes'))
  p<-predict(rf_random$finalModel, test, type="prob")
  auc[method_index, i] <- roc(test.spam, p[,2])$auc
  method_probs[method_index, i, ] <- p[,2]
  
  
  ##################################################################################################################################
  ##################################################################################################################################
  
  
  #Linear Discriminant Analysis
  mnames<-"LDA"
  method_index<-which(method_names%in%mnames)
  lda.fit <- lda(spam ~., data = train)
  lda.pred <- predict(lda.fit, test)$class
  lda.prob <- predict(lda.fit, test)$posterior[,2]
  method_probs[method_index,i,]<-lda.prob
  error[method_index, i] <- mean(lda.pred != test.spam)
  auc[method_index, i] <- roc(test.spam, lda.prob)$auc

  ##################################################################################################################################
  ##################################################################################################################################
  
  
  #SVMlinear
  mnames<-"SVMlinear"
  method_index<-which(method_names%in%mnames)
  allVars <- colnames(train)
  predictorVars <- allVars[!allVars%in%"spam"]
  predictorVars <- paste(predictorVars, collapse ="+")
  form=as.formula(paste("spam~",predictorVars,collapse="+"))
  
  
  train$fold <- caret::createFolds(1:nrow(train), k = 4, list = FALSE)
  ### PARAMETER LIST ###
  cost <- c(10,30,100)
  epsilon <- c(.02,.1,.2)
  parms <- expand.grid(cost = cost, epsilon = epsilon)
  ### LOOP THROUGH PARAMETER VALUES ###
  result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
    c <- parms[i, ]$cost
    e <- parms[i, ]$epsilon
    ### K-FOLD VALIDATION ###
    out <- foreach(j = 1:max(train$fold), .combine = rbind, .inorder = FALSE) %dopar% {
      deve <- train[train$fold != j, ]
      test <- train[train$fold == j, ]
      mdl <- e1071::svm(form, data = deve, type = "C-classification", kernel = "linear", cost = c, epsilon = e, probability = TRUE)
      pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
      data.frame(y = test$spam, prob = attributes(pred)$probabilities[, 2])
    }
    ### CALCULATE SVM PERFORMANCE ###
    roc <- pROC::roc(as.factor(out$y), out$prob) 
    data.frame(parms[i, ], roc = roc$auc[1])
  }
  
  result
  
  c<-result[which.max(result$roc),]$cost
  e<-result[which.max(result$roc),]$epsilon
  mdl <- e1071::svm(form, data = train, type = "C-classification", kernel = "linear", cost = c, epsilon = e, probability = TRUE)
  pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
  method_probs[method_index, i, ] <- attributes(pred)$probabilities[, 2]
  auc[method_index, i]<-pROC::roc(test.spam,attributes(pred)$probabilities[, 2])$auc 
  
  
  ##################################################################################################################################
  ##################################################################################################################################
  

  #SVMpolynomial
  mnames<-"SVMpolynomial"
  method_index<-which(method_names%in%mnames)
  
  #re define data, previous svm may have overwritten this
  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))
  
  #re define formula
  allVars <- colnames(train)
  predictorVars <- allVars[!allVars%in%"spam"]
  predictorVars <- paste(predictorVars, collapse ="+")
  form=as.formula(paste("spam~",predictorVars,collapse="+"))
  
  ## set folds for tuning
  train$fold <- caret::createFolds(1:nrow(train), k = 4, list = FALSE)
  ### PARAMETER LIST ###
  cost <- c(10, 100)
  gamma <- c(1, 2)
  parms <- expand.grid(cost = cost, gamma = gamma)
  ### LOOP THROUGH PARAMETER VALUES ###
  result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
    c <- parms[i, ]$cost
    g <- parms[i, ]$gamma
    ### K-FOLD VALIDATION ###
    out <- foreach(j = 1:max(train$fold), .combine = rbind, .inorder = FALSE) %dopar% {
      deve <- train[train$fold != j, ]
      test <- train[train$fold == j, ]
      mdl <- e1071::svm(form, data = deve, type = "C-classification", kernel = "polynomial", cost = c, gamma = g, probability = TRUE)
      pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
      data.frame(y = test$spam, prob = attributes(pred)$probabilities[, 2])
    }
    ### CALCULATE SVM PERFORMANCE ###
    roc <- pROC::roc(as.factor(out$y), out$prob) 
    data.frame(parms[i, ], roc = roc$auc[1])
  }
  
  result
  c<-result[which.max(result$roc),]$cost
  g<-result[which.max(result$roc),]$gamma
  mdl <- e1071::svm(form, data = train, type = "C-classification", kernel = "polynomial", cost = c, gamma = g, probability = TRUE)
  pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
  method_probs[method_index, i, ] <- attributes(pred)$probabilities[, 2]
  auc[method_index, i]<-pROC::roc(test.spam,attributes(pred)$probabilities[, 2])$auc 
  
  print(paste("Fold number", i, "is now finished.", sep = " "))
}


rowMeans(auc)


##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################





avg_probs<-colMeans(method_probs)
auc_fold <-c()
for (i in 1:cvfold){
  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))
  
  auc1 <- roc(test.spam, avg_probs[i,])
  auc_fold[i] <- auc1$auc
}
mean(auc_fold)
#98.547 with enssemble of all 4

# this did not work so well. it resulted in a lower AUC than GBM alone
# it may be because we used probability instead of majority vote
# it may also be because of correlation between models
# lets check correlation between models and make a better selection for our enssemble




# First lets make data frame/correlation matrix to store values
cor_df <- data.frame(matrix(nrow=length(method_names), ncol=length(method_names)))
row.names(cor_df) <- paste0(method_names)
colnames(cor_df) <- paste0(method_names)

# Most importantly this loop contains code to append folds into a long vector (using melt)
# This allows us to get the correlation between the predictions from methods
# We will need to reset the temp matrix after starting a new method and before going through the cv fold loop
for (i in 1:length(method_names)){ #method loop
  tempx <- tempy <- as.data.frame(matrix(nrow=fold_size, ncol=cvfold))
  for (c in 1:cvfold){ #cv fold loop
    tempx[,c]<-method_probs[i,c,]
  }
  tempx <- melt(tempx, value.name='prob',variable.name='V')[2]
    for (j in 1:length(method_names)){ #method loop 2
      tempy <- as.data.frame(matrix(nrow=fold_size, ncol=cvfold))
      for (v in 1:cvfold){ #cv fold loop for each method of comparison
        tempy[,v]<-method_probs[j,v,]
      }
      tempy <- melt(tempy, value.name='prob',variable.name='V')[2]
      cor_df[i,j]<-cor(tempx ,tempy)
    }
}

#from inspection glm and gbm are too correlated, lets remove glm
rowMeans(auc)

meth2<-method_probs[c(1,2,3,5,7),,]
dim(meth2)
avg_probs<-colMeans(meth2)
auc_fold <-c()
for (i in 1:cvfold){
  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))
  
  auc1 <- roc(test.spam, avg_probs[i,])
  auc_fold[i] <- auc1$auc
}
mean(auc_fold)
#hmm ... it doesn't improve performance
#in other words correlated models don't hurt ...?











error <- cbind(error, rowMeans(error))
auc <- cbind(auc, rowMeans(auc))
false.pos <- cbind(false.pos, rowMeans(false.pos))

column.names <- c(paste0("sim", 1:10), "mean")
colnames(error) <- colnames(auc) <- colnames(false.pos) <- column.names

#change the file name example: CrossValidationTemp2.Rda
save(error, auc, false.pos,
     lambda.l, lambda.r, lambda.e, k.knn, ncomp.pcr, nodes.rf,
     log, plog.l, plog.r, plog.e, lda, sqda, rf, knn, pcr, svm.l, svm.p, svm.r,
     which.lasso, which.ridge, which.elastic,
     file = "CrossValidationDummy_reduced.Rda")

