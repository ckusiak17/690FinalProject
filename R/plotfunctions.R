print_barplot <- function(xvar, fill, plotname){
  pdf(plotname, width = 12, height = 7)
  print(ggplot(spambase, aes_string(x = xvar,
                             fill=fill
  )) +
    geom_bar(stat="bin")
  )
  dev.off()
}