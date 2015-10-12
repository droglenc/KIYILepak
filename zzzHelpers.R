## ===========================================================
## A helper function for making histograms of proportions.
## ===========================================================
kiyiHist <- function(df,breaks,xlim,ylim,clr,len.ticks,freq.ticks,
                     lbl=NULL,lbl.cex=1.25,show.axes=TRUE,...) {
  h1 <- hist(df$tl,breaks=breaks,plot=FALSE,right=FALSE)
  # replace counts with percentages for plotting ... this should be the same
  #   as the density if the density was multiplied by 5 (the width of a bar)
  #   This is easier to control the axis limits.
  n <- sum(h1$counts)
  h1$counts <- h1$counts/n*100
  plot(h1,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",main="",col=clr,...)
  if (show.axes) axis(1,len.ticks,pos=0)
  if (show.axes) axis(2,freq.ticks)
  if (!is.null(lbl)) legend("topright",legend=lbl,bty="n",cex=lbl.cex)
}
