library(ggplot2)

outputdir

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