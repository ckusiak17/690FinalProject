library(ggplot2)

workdir <- "C:/Users/greg/Dropbox/Jfinal_proj"
setwd(workdir)

Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
#Rfiles <- Rfiles[grepl(".R", Rfiles)] # select R scripts in that folder
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file
  # make a output folder to store results from run
  MakeDirs(runname)
output.dir <- MakeDirs(runname)

print_barplot <- function(xvar, fill, plotname){
  pdf(plotname, width = 12, height = 7)
  print(ggplot(spambase, aes(x = xvar,
                     fill=factor(fill)
  )) +
  geom_bar(stat="bin")
  )
  dev.off()
}


getxvar.l <- colnames(spambase)
for(l in 1:length(spambase)) {
  if (getxvar.l[l] != "spam") # plot all predictors and exclude response "spam"
  print_barplot(xvar=getxvar.l[l],fill=spam, plotname=paste0(outputdir,"/fig/barplot.pdf"))
  else
}