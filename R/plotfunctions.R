print_barplot <- function(xvar, fill, plotname){
  pdf(plotname, width = 12, height = 7)
  print(ggplot(spambase, aes(x = xvar,
                             fill=factor(fill)
  )) +
    geom_bar(stat="bin")
  )
  dev.off()
}