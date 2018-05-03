
load("spam.Rda")
###########################
###########################
#step 1 remove response spam
###########################
temp<-spambase
spam<-temp$spam

###########################
###########################
#step 1.1 bin sparse similar data
###########################
#lets add but not remove features, dimensions reduction/feature selection can choose the prefered feature
temp$word_address_bin <- temp$word_address + temp$word_addresses
temp$word_address <- NULL
temp$word_addresses <- NULL
temp$word_you_bin <- temp$word_you +temp$word_your
temp$word_you <- NULL
temp$word_your <- NULL
temp$word_business_bin <- temp$word_business + temp$word_meeting + temp$word_conference + temp$word_project
temp$word_meeting <- NULL
temp$word_conference <- NULL
temp$word_project <- NULL
temp$word_business <- NULL


temp$spam=NULL





###########################
###########################
#step 4 making categorical data
###########################
#make categorical data


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
dummy_data$spam <- spam



