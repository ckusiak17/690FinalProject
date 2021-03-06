#install.packages('sparsediscrim', dependencies = TRUE)
require(sparsediscrim)
require(mosaic)
require(xtable)
require(caret)
require(MASS)
require(kernlab)
require(glmnet)
require(pls)
require(randomForest)
options(xtable.comment = FALSE)
load("spam.Rda")
#source("preprocess.R")

#make an empty matrix to hold the prediction errors
#this is 7 X 10 for the 7 classification methods and the 10 CV folds
error <- auc <- false.pos <- matrix(nrow = 12, ncol = 10)
method_names <- c("Logistic Regression", "LASSO", "Ridge", "Elastic Net", "LDA", 
                  "Sparce QDA", "Random Forests", "KNN", "PCR", "Linear SVM", 
                  "Polynomial SVM", "Radial SVM")
rownames(error) <- rownames(auc) <- rownames(false.pos) <- method_names

lambda.l <- lambda.r <-  lambda.e <- k.knn <- ncomp.pcr <- nodes.rf <- c()
log <- plog.l <- plog.r <- plog.e <- lda <- sqda <- rf <- knn <- pcr <- 
  svm.l <- svm.p <- svm.r <- list()

which.lasso <- which.ridge <- which.elastic <- c()

#change spambase here
set.seed(690)
spam <- mutate(spambase, spam = as.factor(spam), index = sample(rep(1:10, each = 460)))
for (i in 1:10){
  #create a training and test set
  #ADD LASSO

  train <- subset(filter(spam, index != i), select = -index)
  test.spam <- filter(spam, index == i)$spam
  test <- subset(filter(spam, index == i), select = - c(index, spam))

  logistics <- lapply(subset(train, select = -spam), 
                      function(x) summary(glm(train$spam ~ x, family= "binomial"))$coefficients[2,4] )
  log.index <- as.vector(logistics < 0.05)
  train <- train[, log.index]
  test <- test[, log.index]
  
  #logistic regression
  log.fit <- glm(spam ~ ., data = train, famil= binomial)
  log.prob <- predict(log.fit, test, type = "response")
  log.pred <- ifelse(log.prob > .5, 1, 0)
  error[1, i] <- mean(log.pred != test.spam)
  auc[1, i] <- auc(log.pred, test.spam)
  false.pos[1, i] <- table(log.pred, test.spam)[2,1]/
    (table(log.pred, test.spam)[2,1] + table(log.pred, test.spam)[1,1])
  log[[i]] <- log.fit


  #data set up for penalized logistic regressions
  x <- as.matrix(subset(train, select = -spam)) #removes spam from the dataframe
  x.test <- as.matrix(test)

  #lasso penalty, alpda = 1
  plog.l.fit <- glmnet(x, train$spam, family = "binomial", alpha=1, standardize = TRUE)
  plog.l.prob <- predict(plog.l.fit, x.test, type = "response")
  plog.l.pred <- ifelse(plog.l.prob > .5, 1, 0)
  l.error <- c()
  l.false <- c()
  for (lambda in 1:ncol(plog.l.pred)){
    l.error[lambda] <-  mean(plog.l.pred[,lambda] != test.spam)
    l <- data.frame(pred = plog.l.pred[,lambda], true = test.spam)
    l.false[lambda] <- nrow(filter(l, pred == 1, true == 0))/nrow(filter(l, true == 0))
    }
  error[2, i] <- min(l.error) #chooses error for best penalty
  false.pos[2, i] <- l.false[which.min(l.error)]
  which.lasso[i] <- which.min(l.error)
  auc[2, i] <- auc(plog.l.pred[,which.min(l.error)], test.spam)
  lambda.l[i] <- plog.l.fit$lambda[which.min(l.error)] #identifies best lambda from model fit
  plog.l[[i]] <- plog.l.fit

  #ridge penalty, alpha = 0
  plog.r.fit <- glmnet(x, train$spam, family = "binomial", alpha = 0)
  plog.r.prob <- predict(plog.r.fit, x.test, type = "response")
  plog.r.pred <- ifelse(plog.r.prob > .5, 1, 0)
  r.error <- c()
  r.false <- c()
  for (lambda in 1:ncol(plog.r.pred)){
    r.error[lambda] <-  mean(plog.r.pred[,lambda] != test.spam)
    r <- data.frame(pred = plog.r.pred[,lambda], true = test.spam)
    r.false[lambda] <- nrow(filter(r, pred == 1, true == 0))/nrow(filter(r, true == 0))
    }
  error[3, i] <- min(r.error)
  auc[3, i] <- auc(plog.r.pred[,which.min(r.error)], test.spam)
  which.ridge[i] <- which.min(r.error)
  false.pos[3, i] <- r.false[which.min(r.error)]
  lambda.r[i] <- plog.r.fit$lambda[which.min(r.error)]
  plog.r[[i]] <- plog.r.fit

  #elastic net penalty with equal mixing, alpha = 0.5
  plog.e.fit <- glmnet(x, train$spam, family = "binomial", alpha = 0.5)
  plog.e.prob <- predict(plog.e.fit, x.test, type = "response")
  plog.e.pred <- ifelse(plog.e.prob > .5, 1, 0)
  e.error <- c()
  e.false <- c()
  for (lambda in 1:ncol(plog.e.pred)){
    e.error[lambda] <-  mean(plog.e.pred[,lambda] != test.spam)
    e <- data.frame(pred = plog.e.pred[,lambda], true = test.spam)
    e.false[lambda] <- nrow(filter(e, pred == 1, true == 0))/nrow(filter(e, true == 0))
    }
  error[4, i] <- min(e.error)
  auc[4, i] <- auc(plog.e.pred[,which.min(e.error)], test.spam)
  which.elastic[i] <- which.min(e.error)
  false.pos[4, i] <- e.false[which.min(e.error)]
  lambda.e[i] <- plog.e.fit$lambda[which.min(e.error)]
  plog.e[[i]] <- plog.e.fit


  #Linear Discriminant Analysis
  lda.fit <- lda(spam ~., data = train)
  lda.pred <- predict(lda.fit, test)$class
  error[5, i] <- mean(lda.pred != test.spam)
  auc[5, i] <- auc(ifelse(lda.pred == 1, 1, 0), test.spam) #needed to convert from factor
  false.pos[5, i] <- table(lda.pred, test.spam)[2,1]/
    (table(lda.pred, test.spam)[2,1] + table(lda.pred, test.spam)[1,1])
  lda[[i]] <- lda.fit


  #sparce QDA
  sqda.fit <- dqda(spam~.,data=train)
  sqda.prob <- predict(sqda.fit, newdata=test)
  sqda.pred <- sqda.prob$class
  error[6, i] <- mean(sqda.pred != test.spam)
  auc[6, i] <- auc(ifelse(sqda.pred == 1, 1, 0), test.spam) #needed to convert from factor
  false.pos[6, i] <- table(sqda.pred, test.spam)[2,1]/
    (table(sqda.pred, test.spam)[2,1] + table(sqda.pred, test.spam)[1,1])
  sqda[[i]] <- sqda.fit
  
  #random forests
  rf.fit <- randomForest(formula = spam ~ ., data =  train)
  rf.pred <- predict(rf.fit, test)
  error[7, i] <- mean(rf.pred != test.spam)
  auc[7, i] <- auc(ifelse(rf.pred == 1, 1, 0), test.spam) #needed to convert from factor
  false.pos[7, i] <- table(rf.pred, test.spam)[2,1]/(table(rf.pred, test.spam)[2,1] + table(rf.pred, test.spam)[1,1])
  nodes.rf[i] <- rf.fit$forest$nrnodes
  rf[[i]] <- rf.fit
  
  #K-Nearest Neighbors
  #this runs a 10 fold cross validation to determine
  #which value of k is optimal for knn
  trControl <- trainControl(method  = "cv",
                            number  = 10)
  knn.fit <- train(spam ~ .,
                   method     = "knn",
                   tuneGrid   = expand.grid(k = 1:10),
                   trControl  = trControl,
                   metric     = "Accuracy",
                   data       = train)
  knn.pred <- predict(knn.fit, test)
  error[8, i] <- mean(knn.pred != test.spam)  
  auc[8, i] <- auc(ifelse(knn.pred == 1, 1,0 ), test.spam)
  false.pos[8, i] <- table(knn.pred, test.spam)[2,1]/(table(knn.pred, test.spam)[2,1] + table(knn.pred, test.spam)[1,1])
  k.knn[i] <- knn.fit$bestTune[1,1]
  knn[[i]] <- knn.fit
  
  #Principle Components Regression
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
  pcr.pred <- ifelse(pcr.prob > .5, 1, 0)
  error[9, i] <- mean(pcr.pred != test.spam)
  auc[9, i] <- auc(pcr.pred, test.spam)
  false.pos[9, i] <- table(pcr.pred, test.spam)[2,1]/(table(pcr.pred, test.spam)[2,1] + table(pcr.pred, test.spam)[1,1])
  pcr[[i]] <- pcr.fit
  
  #each of the svm methods also require an optimal value for C, 
  #the train() function here runs CV for this determination
  svm.l.fit <- train(spam ~., data = train, 
                     method = "svmLinear",
                     uneGrid   = expand.grid(c = 1:10),
                     trControl=trControl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)
  svm.l.pred <- predict(svm.l.fit, test)
  error[10, i] <- mean(svm.l.pred != test.spam)
  auc[10, i] <- auc(ifelse(svm.l.pred ==1, 1, 0 ), test.spam)
  false.pos[10, i] <- table(svm.l.pred, test.spam)[2,1]/
    (table(svm.l.pred, test.spam)[2,1] + table(svm.l.pred, test.spam)[1,1])
  svm.l[[i]] <- svm.l.fit
  
  
  svm.p.fit <- train(spam ~., data = train, 
                     method = "svmPoly",
                     uneGrid   = expand.grid(c = 1:10),
                     trControl=trControl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)
  svm.p.pred <- predict(svm.p.fit, test)
  error[11, i] <- mean(svm.p.pred != test.spam)
  auc[11, i] <- auc(ifelse(svm.p.pred ==1, 1, 0 ), test.spam)
  false.pos[11, i] <- table(svm.p.pred, test.spam)[2,1]/
    (table(svm.p.pred, test.spam)[2,1] + table(svm.p.pred, test.spam)[1,1])
  svm.p[[i]] <- svm.p.fit
  
  svm.r.fit <- train(spam ~., data = train, 
                     method = "svmRadial",
                     uneGrid   = expand.grid(c = 1:10),
                     trControl=trControl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)
  svm.r.pred <- predict(svm.r.fit, test)
  error[12, i] <- mean(svm.r.pred != test.spam)
  auc[12, i] <- auc(ifelse(svm.r.pred ==1, 1, 0 ), test.spam)
  false.pos[12, i] <- table(svm.r.pred, test.spam)[2,1]/
    (table(svm.r.pred, test.spam)[2,1] + table(svm.r.pred, test.spam)[1,1])
  svm.r[[i]] <- svm.r.fit
  
  print(paste("Fold number", i, "is now finished.", sep = " "))

}

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
     file = "CrossValidationReduced.Rda")