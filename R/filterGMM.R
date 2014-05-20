#' Filter peptide microarray data
#' 
#' filterGMM is a function that filters peptide array data by identifying probes 
#' that specifically bind the secondary antibody and probes that are unreactive 
#' on the sample array. It does this by fitting a 2 component gaussian mixture model to the 
#' signal data from an array incubated with just secondary antibody (control array).
#' Probes with signal exceeding the value of the cutoff quantile are flagged 
#' as secondary antibody binders. 
#' 
#' @param arraydata Vector of log transformed normalised signal intensities from 
#' the sample array
#' @param controldata Vector of log transformed normalised signal intensities from 
#' the control array (incubated with just secondary antibody). Must be same the 
#' length as arraydata.
#' @param cutoff.quantile Value indicating quantile of 1st gaussian distribution
#' to set as a cutoff point. Defaults to 0.99
#' @return logical vector of the same length as arraydata 
#' (TRUE = valid probes, FALSE = filtered probes)
#' @import mixtools
#' @import RColorBrewer
#' @export
filterGMM <- function(arraydata, controldata, cutoff.quantile = 0.99, plot = TRUE, 
                      lhist = 50, num.dnorm=5*lhist, xlab = "", ylab = "", 
                      plottitle="", xsize=1, 
                      colramp = colorRampPalette(rev(brewer.pal(11, "RdYlBu"))),
                      cleanup=TRUE,...){
  
  set.seed(100)
  control.gmm <- normalmixEM(controldata, k = 2, maxit = 1000, epsilon = 1e-08, fast = TRUE)
  control.cutoff <- qnorm(cutoff.quantile, control.gmm$mu[1], control.gmm$sig[1])
  
  set.seed(100)
  array.gmm <- normalmixEM(arraydata, k = 2, maxit = 1000, epsilon = 1e-08, fast = TRUE)
  array.cutoff <- qnorm(cutoff.quantile, array.gmm$mu[1], array.gmm$sig[1])
  
  filterdata <- controldata < control.cutoff & arraydata > array.cutoff
  
  if (plot){
    def.par <- par(no.readonly = TRUE)
    
    # Define plotting areas for scatterplot and marginal histograms
    zones <- matrix(c(1,1,1, 0,5,0, 2,6,4, 0,3,0), ncol = 3, byrow = TRUE)
    layout(zones, widths=c(0.3,4,1), heights = c(1,3,10,.75))
    par(xaxt="n", yaxt="n",bty="n",  mar = c(.3,2,.3,0) +.05)
    
    # Main Title
    plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
    text(0,0,paste(plottitle), cex=2)
    # Y Axis Title
    plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
    text(0,0,paste(ylab), cex=1.5, srt=90)
    # X Axis Title
    plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
    text(0,0,paste(xlab), cex=1.5)
    
    # Y Azis Histogram
    par(mar = c(2,0,1,1)) # no margin on the left
    yhist <- hist(arraydata, plot=FALSE, breaks=seq(from=min(arraydata), to=max(arraydata), length.out=lhist))
    barplot(yhist$density, axes = FALSE, xlim = c(0, max(yhist$density)), space=0, horiz = TRUE)
    
    # Y Axis Density Plots (GMM)
    yy <- seq(min(arraydata), max(arraydata), length.out=num.dnorm)
    ydens1 <- array.gmm$lambda[1] * dnorm(yy, mean = array.gmm$mu[1], sd = array.gmm$sigma[1]) 
    ydens2 <- array.gmm$lambda[2] * dnorm(yy, mean = array.gmm$mu[2], sd = array.gmm$sigma[2])
    lines(ydens1, seq(from=0, to=lhist-1, length.out=num.dnorm),lwd = 1.5, col= "blue")
    lines(ydens2, seq(from=0, to=lhist-1, length.out=num.dnorm), lwd = 1.5, col= "red")
    
    # X Axis Histogram
    par(mar = c(0,2,1,1))
    xhist <- hist(controldata, plot=FALSE, breaks=seq(from=min(controldata), to=max(controldata), length.out=lhist))
    barplot(xhist$density, axes = FALSE, ylim = c(0, max(xhist$density)), space=0)
    
    # X Axis Density Plots (GMM)
    xx <- seq(min(controldata), max(controldata), length.out=num.dnorm)
    xdens1 <- control.gmm$lambda[1] * dnorm(xx, mean = control.gmm$mu[1], sd = control.gmm$sigma[1]) 
    xdens2 <- control.gmm$lambda[2] * dnorm(xx, mean = control.gmm$mu[2], sd = control.gmm$sigma[2])
    lines(seq(from=0, to=lhist-1, length.out=num.dnorm), xdens1, lwd = 1.5, col= "blue")
    lines(seq(from=0, to=lhist-1, length.out=num.dnorm), xdens2, lwd = 1.5, col= "red")
    
    # Smooth Scatter Plot
    par(mar = c(2,2,.5,.5), xaxt="s", yaxt="s", bty="n")
    smoothScatter(controldata, arraydata, 
                  nbin = 256,
                  bandwidth = 0.3,
                  pch = 20, 
                  colramp = colramp, 
                  nrpoints = Inf, 
                  cex = 0.1,
                  las = 1,
                  xlab = "",
                  ylab = "",
                  ...
    )
    
    x.cutoff <- qnorm(cutoff.quantile, control.gmm$mu[1], control.gmm$sig[1])
    abline(v = x.cutoff)
    
    y.cutoff <- qnorm(cutoff.quantile, array.gmm$mu[1], array.gmm$sig[1])
    abline(h = y.cutoff)
    
    if(cleanup) {par(def.par)}
  }
  return(filterdata)
}