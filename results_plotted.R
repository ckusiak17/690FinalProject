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
library(doMC) # I don't remmeber if this is needed or was for a seperate attempt
registerDoMC()
options(xtable.comment = FALSE)
#load("spam.Rda")
source("preprocess.R")
load("spam.Rda")

##################################################################################################################################
##################################################################################################################################


# Adding models
#cont
#method_names <- c( "LASSO", "KNN", "PCR", "RandomForest", "LDA", "SVMlinear")
#dummy
method_names <- c( "LASSO", "KNN", "PCR", "RandomForest", "LDA", "SVMpolynomial", "SVMlinear")


#lets add a dimension to our array in order to add a new model which was run in a seperate script
#we are adding gbm prob
load('Ensemble_results.Rda')
load('Ensemble_results_cont.Rda')
load('gbm_results.Rda')
load('gbm_results_cont.Rda')
method_probs2 <- array(dim=c(length(method_names)+8, cvfold, fold_size)) #+6 for cont +2 for gbms = 8
method_probs2[-c(8:15),,] <- method_probs # add the dummy results first
method_probs2[-c(1:7,14,15),,] <- method_probs_cont # add the cont results
method_probs2[14,,] <- t(gbm_prob)
method_probs2[15,,] <- t(gbm_prob_cont)




method_probs <- method_probs2
method_names <-  c( "LASSO", "KNN", "PCR", "RandomForest", "LDA", "SVMpolynomial", "SVMlinear", "LASSOcont", "KNNcont", "PCRcont", "RandomForestcont", "LDAcont", "SVMlinearcont","GBM","GBMcont")
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
#meth2<-method_probs[c(-5,-6,-7,-8),,] the best prior to adding cont
meth2<-method_probs[c(1,2,3,4,8,10,11,15)  ,,]
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
ensemble <- auc_fold
mean(auc_fold)
ensemble_result <- mean(auc_fold)

#Great there may be a better way to search for the best model combination but this works for now
#Lets make a loop to retrieve auc per fold for each method
#we need to create the test.spam inside the loop so first lets make folds exactly the way they are in the model scripts
set.seed(690)
cvfold <- 10
fold_size <- nrow(dummy_data)/cvfold
spam <- mutate(dummy_data, spam = as.factor(spam), index = sample(rep(1:cvfold, each = fold_size)))

auc_store<-data.frame(matrix(nrow=10, ncol=15))
colnames(auc_store)<-method_names
row.names(auc_store)<-1:10
for (i in 1:15 ){ #for each model
  for (k in 1:10) { #for each fold
    train <- subset(filter(spam, index != k), select = -index)
    test.spam <- filter(spam, index == k)$spam

    auc_store[k,i] <- roc(test.spam,method_probs2[i,k,])$auc
  }
}

auc_store$ensemble<-ensemble
write.csv(auc_store, "auc_all_methods.csv")


#OK PLOTTING
library(tidyr)
auc_store$fold <- 1:10
auc_clean <- auc_store %>% 
  gather(method, auc, c(method_names,"ensemble")) 

library(ggplot2)
# Basic line plot with points

ggplot() +
  geom_line(data=auc_clean, aes(x=fold, y=auc, color=method), size = 1.5) +
  geom_line()+
  geom_point(size=1.5)+
  ggtitle("All Methods vs Ensemble")+ 
  scale_x_discrete(name ="Fold", limits=c(1:10))




#Now lets plot with the variables used to ensemble
auc_store2<-auc_store[,c(1,2,3,4,8,10,11,15)]
auc_store2$ensemble<-ensemble
auc_store2$fold <- 1:10
auc_clean <- auc_store2 %>% 
  gather(method, auc, c(method_names[c(1,2,3,4,8,10,11,15)],"ensemble")) 

# Basic line plot with points
ggplot() +
  geom_line(data=auc_clean, aes(x=fold, y=auc, color=method), size = 1.5) +
  geom_line()+
  geom_point(size=1.5)+
  ggtitle("Ensemble Methods")+ 
  scale_x_discrete(name ="Fold", limits=c(1:10))
