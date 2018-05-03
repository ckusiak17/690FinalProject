
load("spam.Rda")

###########################
###########################
#step 1 remove response spam
###########################
temp<-spambase
spam<-temp$spam
temp$spam=NULL


###########################
###########################
#step 2 transform data
###########################
#function to normalize variables ... not sure this is useful
log_1 <-function(x){
  x<-log(x+1)
}
#sapply this function to our entire data set, excluding spam variable
temp2<-as.data.frame(sapply(temp, log_1))
#check to see it worked
hist(temp$word_your)
hist(temp2$word_your)
#now lets scale data so each column has mean 0 and sd of 1

center_apply <- function(x) {
  apply(x, 2, function(y) ((y - mean(y))/sd(y)))
}
scaled_temp <- scale(temp2)
scaled_temp <- center_apply(temp2)
#check that we get mean of 0 and sd of 1
colMeans(scaled_temp)




###########################
###########################
#step 3 make features, kmeans center distance
###########################
#for the future when we apply all of this code to a better data set with IP address and time/date (insert any sparse category)
#entropy <- function( numberPerName ) {
#  tot <- sum(numberPerName)
#  p   <- numberPerName/tot
#  W   <- -sum(p*log(p))/log(2)
#  return(W)
#}



set.seed(20)
kmeans.result <- kmeans(scaled_temp, 3)
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((scaled_temp - centers)^2))
summary(distances)
#now lets add this feature to our data
#temp2$center_distance<-distances
#temp$center_distance<-distances
#we dont need some global env stuff so lets get rid of it
#this is good practice because global objects use up RAM, slow down R
scaled_temp <- NULL
kmeans.result <- NULL
centers <- NULL
spambase <- NULL



###########################
###########################
#step 4 making categorical data
###########################
#make categorical data
toop <- c()
for (i in 1:length(temp)) {
  x <- temp[,i] #save one variable at a time
  toop[i]<-(sum(x==0)>((3/4)*nrow(temp)))
}
sum(toop)
#43 variables are 3/4 zeros!

get_quantiles <-function(x) quantile(x=x,seq(0,1,.25))
get_quantiles_zero_2 <- function(x) quantile(x=x[x!=0], seq(0,1,.5))
#this function will output a matrix called data which is categorical
cut_columns <- function(cat_data) {
  for (i in 1:length(cat_data)) {
    x <- cat_data[,i] #save one variable at a time
    if (sum(x==0)>((3/4)*nrow(cat_data))) { #extreme case, added this after the process so it doesnt have a name lol
      logic <- x==0
      x[!logic] <- 1
      newx <- x
            } else if (sum(x==0)>((1/4)*nrow(cat_data))) { #zero2
              qnt <- get_quantiles_zero_2(x) #get quantiles of saved variable
              newx <- cut(x, c(0,unique(qnt)), include.lowest = TRUE) #cut into categories using unique percentiles
              newx <- factor(newx, labels=1:length(levels(newx))) #gives simple numeric names to the levels instead of the quantile value... just looks nice
              } else { #not zero inflated
                qnt <- get_quantiles(x) #get quantiles of saved variable
                newx <- cut(x, unique(qnt), include.lowest = TRUE) #cut into categories using unique percentiles
                newx <- factor(newx, labels=1:length(levels(newx))) #gives simple numeric names to the levels instead of the quantile value... just looks nice
              }

    cat_data[,i] <<- newx #replace the numeric variable in the data with our new categorical variable
  }#end loop
}#end function

#we have to create an empty matrix first.... if you have a better way to do this please tell me. this seems sloppy
cat_data <- matrix(nrow=nrow(temp), ncol=ncol(temp))
cut_columns(temp) # gives us a matrix called data which is categorical
cat_data <- as.data.frame(cat_data)#make a data frame
colnames(cat_data)<-colnames(temp)#assign column names


###########################
###########################
#step 5 dummy encode (aka one hot encode) our categorical data
###########################
# We can only use categories here
cat_data<-sapply(cat_data, factor)
library(caret)
# We need dummy variables for an elastic net
dummy<-dummyVars(~.,data=cat_data, fullRank = TRUE) #get the dummy variables
dummy_data<-as.data.frame(predict(dummy, cat_data)) #this just puts it in a data frame and gives nice column names


temp$spam <- spam
temp2$spam <- spam
dummy_data$spam <- spam


#library(ggplot2)
#ggplot(temp, aes(x =center_distance,
#                 fill=factor(spam)
#)) +
#  geom_bar(stat="bin")

#temp$outlier_not<-ifelse(temp$center_distance>15,0,1)
#summary(temp$center_distance)
#which.max(temp$center_distance)
#sum(!temp$outlier&temp$spam)


