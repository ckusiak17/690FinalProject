print_barplot <- function(xvar, fill, plotname){
  pdf(plotname, width = 12, height = 7)
  print(ggplot(spambase, aes_string(x = xvar,
                             fill=fill
  )) +
    geom_bar(stat="bin")
  )
  dev.off()
}


print_boxplot <- function(xvar,yvar,plotname) {
  pdf(plotname, width = 12, height = 7)
  print(ggplot(spambase, aes_string(x = xvar, y = yvar
                          )) +  
  geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)
  )
  dev.off()
}



print_plots <- function(xvar,category,plotname) {
  pdf(plotname, width = 12, height = 7)
  print(ggplot(spambase, aes_string(x = xvar,
                                    fill=category
  )) +
    geom_bar(stat="bin")
  )
  print(ggplot(spambase, aes_string(x = category, y = xvar, fill = category
  )) +  
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE)
  )
  dev.off()
}