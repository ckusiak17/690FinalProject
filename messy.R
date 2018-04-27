


workdir <- "C:/Users/greg/Documents/690FinalProject"
setwd(workdir)
runname <- "greg" # used to later to create a folder and directory for a specific run


Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
  #Rfiles <- Rfiles[grepl(".R", Rfiles)] # select R scripts in that folder
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file
  # make a output folder to store results from run
MakeDirs(runname)
outputdir <- MakeDirs(runname)



library(ggplot2)

getxvar.l <- colnames(spambase)
for(l in 1:length(spambase)) {
  if (getxvar.l[l] != "spam") # plot all predictors and exclude response "spam"
  print_barplot(xvar=getxvar.l[l],fill="spam", plotname=paste0(outputdir,"/barplot.pdf"))
}