
require(mosaic)
require(xtable)
require(caret)
require(MASS)
require(kernlab)
require(glmnet)
require(pls)
require(penalizedSVM)
options(xtable.comment = FALSE)
load("spam.Rda")

#make an empty matrix to hold the prediction errors
#this is 7 X 10 for the 7 classification methods and the 10 CV folds
error <- auc <- false.pos <- matrix(nrow = 11, ncol = 10)
method_names <- c("Logistic Regression", "LASSO", "Ridge", "Elastic Net", 
"LDA", "QDA", "KNN", "PCR", "Linear SVM", "Polynomial SVM",
"Radial SVM")
rownames(error) <- rownames(auc) <- rownames(false.pos) <- method_names

lambda.l <- lambda.r <-  lambda.e <- k.knn <- ncomp.pcr <- c()
log <- plog.l <- plog.r <- plog.e <- lda <- qda <- knn <- pcr <- 
svm.l <- svm.p <- svm.r <- psvm.l <- psvm.e <- psvm.s <- list()

which.lasso <- c()


spam <- mutate(spambase, spam = as.factor(spam), index = sample(rep(1:10, each = 460)))
for (i in 1:10){
#create a training and test set
#[,-59] removes the index variable
#ADD LASSO

training <- filter(spam, index == i)[,-59]
train.spam <- training$spam
x <- as.matrix(training[,-58])
test.spam <- filter(spam, index != i)$spam
testing <- filter(spam, index != i)[,-c(58,59)]
lass <- cv.glmnet(x, train$spam, alpha = 1, family = "binomial")
zero.index <- which(coef(lass) == 0) -1
train <- training[,-zero.index]
test <- testing[,-zero.index]

#logistic regression
log.fit <- glm(spam ~ ., data = train, famil= binomial)
log.prob <- predict(log.fit, test, type = "response")
log.pred <- ifelse(log.prob > .5, 1, 0)
error[1, i] <- mean(log.pred != test.spam)
auc[1, i] <- auc(log.pred, test.spam)
false.pos[1, i] <- table(log.pred, test.spam)[2,1]/nrow(test)
log[[i]] <- log.fit


#data set up for penalized logistic regressions
x <- as.matrix(subset(train, select  = - spam)) #removes spam from the dataframe
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
l.false[lambda] <- nrow(filter(l, pred == 1, true == 0))/nrow(test)
}
error[2, i] <- min(l.error) #chooses error for best penalty
false.pos[2, i] <- l.false[which.min(l.error)]
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
r.false[lambda] <- nrow(filter(r, pred == 1, true == 0))/nrow(test)
}
error[3, i] <- min(r.error)
auc[3, i] <- auc(plog.r.pred[,which.min(r.error)], test.spam)
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
e.false[lambda] <- nrow(filter(e, pred == 1, true == 0))/nrow(test)
}
error[4, i] <- min(e.error)
auc[4, i] <- auc(plog.e.pred[,which.min(e.error)], test.spam)
false.pos[4, i] <- e.false[which.min(e.error)]
lambda.e[i] <- plog.e.fit$lambda[which.min(e.error)]
plog.e[[i]] <- plog.e.fit

#Linear Discriminant Analysis
lda.fit <- lda(spam ~., data = train)
lda.pred <- predict(lda.fit, test)$class
error[5, i] <- mean(lda.pred != test.spam)
auc[5, i] <- auc(ifelse(lda.pred == 1, 1, 0), test.spam) #needed to convert from factor
false.pos[5, i] <- table(lda.pred, test.spam)[2,1]/nrow(test)
lda[[i]] <- lda.fit

#Quadratic Discriminant Analysis
#qda.fit <- qda(spam ~., data = train)
#qda.pred <- predict(qda.fit, test)$class
#error[6, i] <- mean(qda.pred != test.spam)
#auc[6, i] <- auc(ifelse(qda.pred == 1, 1, 0), test.spam)
#false.pos[6, i] <- table(qda.pred, test.spam)[2,1]/nrow(test)
#qda[[i]] <- qda.fit


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
error[7, i] <- mean(knn.pred != test.spam)  
auc[7, i] <- auc(ifelse(knn.pred == 1, 1,0 ), test.spam)
false.pos[7, i] <- table(knn.pred, test.spam)[2,1]/nrow(test)
k.knn[i] <- knn.fit$bestTune[1,1]
knn[[i]] <- knn.fit

#Principle Components Regression
# nofactor.spam <- mutate(spam, spam = ifelse(spam == 1, 1, 0))
# nofactor.train <- filter(nofactor.spam, index == i)[,-59]
# nofactor.train <- nofactor.train[,-zero.index]
# nofactor.test.spam <- filter(nofactor.spam, index != i)$spam
# nofactor.test <- filter(nofactor.spam, index != i)[,-c(58,59)]
# nofactor.test <- nofactor.test[,-zero.index]
# train.matrix <- model.matrix(spam ~. , data = nofactor.train)[,-1]
# train.spam <- nofactor.train$spam
# pcr.fit <- pcr(spam ~., data = nofactor.train, scale = TRUE, validation = "CV")
# aa1 <- pcr.fit[["validation"]][["PRESS"]]
# ncomp.pcr[i] <- which(aa1==min(aa1))
# pcr.prob <- predict(pcr.fit, nofactor.test,  ncomp = which(aa1==min(aa1)))
# pcr.pred <- ifelse(pcr.prob > .5, 1, 0)
# error[8, i] <- mean(pcr.pred != test.spam)
# auc[8, i] <- auc(pcr.pred, test.spam)
# false.pos[8, i] <- table(pcr.pred, test.spam)[2,1]/nrow(test)
# pcr[[i]] <- pcr.fit

#each of the svm methods also require an optimal value for C, 
#the train() function here runs CV for this determination
svm.l.fit <- train(spam ~., data = train, 
method = "svmLinear",
uneGrid   = expand.grid(c = 1:10),
trControl=trControl,
preProcess = c("center", "scale"),
tuneLength = 10)
svm.l.pred <- predict(svm.l.fit, test)
error[9, i] <- mean(svm.l.pred != test.spam)
auc[9, i] <- auc(ifelse(svm.l.pred ==1, 1, 0 ), test.spam)
false.pos[9, i] <- table(svm.l.pred, test.spam)[2,1]/nrow(test)
svm.l[[i]] <- svm.l.fit


#svm.p.fit <- train(spam ~., data = train, 
#method = "svmPoly",
#uneGrid   = expand.grid(c = 1:10),
#trControl=trControl,
#preProcess = c("center", "scale"),
#tuneLength = 10)
#svm.p.pred <- predict(svm.p.fit, test)
#error[10, i] <- mean(svm.p.pred != test.spam)
#auc[10, i] <- auc(ifelse(svm.p.pred ==1, 1, 0 ), test.spam)
#false.pos[10, i] <- table(svm.p.pred, test.spam)[2,1]/nrow(test)
#svm.p[[i]] <- svm.p.fit

#svm.r.fit <- train(spam ~., data = train, 
#method = "svmRadial",
#uneGrid   = expand.grid(c = 1:10),
#trControl=trControl,
#preProcess = c("center", "scale"),
#tuneLength = 10)
#svm.r.pred <- predict(svm.r.fit, test)
#error[11, i] <- mean(svm.r.pred != test.spam)
#auc[11, i] <- auc(ifelse(svm.r.pred ==1, 1, 0 ), test.spam)
#false.pos[11, i] <- table(svm.r.pred, test.spam)[2,1]/nrow(test)
#svm.r[[i]] <- svm.r.fit


#psvm.l.fit <- svm.fs(x=train.matrix, y=ifelse(train$spam == 0, -1, 1), 
#fs.method="1norm", lambda1.set=Lambda.scad)


}

errorLASSO <- cbind(error, rowMeans(error))
aucLASSO <- cbind(auc, rowMeans(auc))
false.posLASSO <- cbind(false.pos, rowMeans(false.pos))

column.names <- c(paste0("sim", 1:10), "mean")
colnames(errorLASSO) <- colnames(aucLASSO) <- colnames(false.posLASSO) <- column.names

save(errorLASSO, aucLASSO, false.posLASSO, 
     lambda.l, lambda.r, lambda.e, k.knn, ncomp.pcr,
     log, plog.l, plog.r, plog.e, lda, qda, knn, pcr, svm.l, svm.p, svm.r,
     file = "CrossValidationLASSO.Rda")

