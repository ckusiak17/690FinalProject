library(sparseSVM)

train.size <- .8*nrow(dummy_data)
rows <- 1:nrow(dummy_data)
train.rows<-sample(rows, train.size, replace=F)
test.rows<-subset(rows, !rows%in%train.rows)
training.set <- dummy_data[train.rows,]
training.set$spam <- NULL
test.set<- dummy_data[test.rows,]
test.set$spam<-NULL
#for repsonse
training.set.y <- spam[train.rows]
test.set.y <- spam[test.rows]


cvfit <- cv.sparseSVM(as.matrix(training.set), training.set.y,
             ncores = 3, eval.metric = c("me"),
             nfolds = 10, trace = FALSE)

which.min(cvfit$cve)
cvfit$lambda[65]
cvfit$lambda.min

pred<-predict(cvfit, as.matrix(test.set), lambda=cvfit$lambda.min)
category<-test.set.y
table(category, pred)
sum(pred==category)/length(category) # classification rate 94.34% correct
