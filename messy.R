

#workdir <- getwd() # this problably works isntead of the next line for you two
workdir <- "C:/690FinalProject" #  my directory, put yours here and comment this out termporarily
setwd(workdir)
runname <- "greg" # used to later to create a folder and directory for a specific run

Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
  #Rfiles <- Rfiles[grepl(".R", Rfiles)] # select R scripts in that folder
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file
  # make a output folder to store results from run
MakeDirs(runname)
outputdir <- MakeDirs(runname)


# This is just a loop that makes plots for each variables using a small function
# plots sent to project/output/greg/fig folder for my run of this
library(ggplot2)
for(l in 1:length(spambase)) {
  if (getxvar.l[l] != "spam") # plot all predictors and exclude response "spam"
    print_plots(xvar=getxvar.l[l],category="spam", plotname=paste0(outputdir,"/fig/box_",getxvar.l[l],".pdf"))
}