library("penalizedSVM")
#quickly make a training and a test set 80/20 just because thats easy
#for predictors
train.size <- .8*nrow(dummy_data)
rows <- 1:nrow(dummy_data)
train.rows<-sample(1:rows, train.size, replace=F)
test.rows<-subset(rows, !rows%in%train.rows)
training.set <- dummy_data[train.rows,]
test.set<- dummy_data[test.rows,]
#for repsonse
logic<-spam==0
spam[logic]<--1
training.set.y <- spam[train.rows]
test.set.y <- spam[test.rows]

#now lets model
fit<-svm.fs(
  x=as.matrix(training.set),
  y=training.set.y,
  fs.method = "scad")
  


#here is the same model with parameters selected (discretelambda search), this should run faster
fit<-svm.fs(
  x=as.matrix(training.set),
  y=training.set.y,
  fs.method = "scad",
### tuning parameter settings
# chose the search method for tuning lambda1,2: 'interval' or 'discrete'
grid.search="discrete",
#fixed grid for lambda1, lambda2
# define range for lambda1,2 for interval search
lambda1.set=.05,
lambda2.set=.01,
# parms.coding="none" or "log2"
parms.coding= "none",
# internal parameter for DIRECT
### valuidation settings
# fot nested validation, 'cross.outer'-fold cv
#cross.outer= 0,
# method for the inner validation: cross validation, gacv
inner.val.method = "cv",
# 'cross.inner'-fold cv
cross.inner= 5,
# show plots in Direct?
show= "final",
### other settings
#seed
seed=123,
# max iterations for the feature selection svm method
maxIter=300,
# verbose?
verbose=TRUE)


preds<-predict(fit, newdata=test.set, newdata.labels = NULL, labels.universe=NULL)

summary(preds$fitted)



