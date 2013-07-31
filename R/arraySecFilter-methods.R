#' Filter Secondary Antibody Binding Peptide Probes
#' 
#' Implements local background correction for peptide microarray data
#' 
#' @param x MultiSet object with fMedian and bMedian matrices in the assayData slot
#' @param method Character string specifying background correction method. 
#' Valid methods are 'none', 'subtract', 'edwards', ratio' or 'normexp'. Defaults to 'none' if no method is specified.
#' @param offset numeric value added to raw signal intensity before background correction is implemented
#' @param transform Expression to transform raw data. Defaults to log2
#' @return MultiSet object with transformed and background corrected foreground signal in the fMedian matrix
#'  
#' @export
#' @docType methods
#' @rdname arraySecFilter-methods
setGeneric(
  name = "arraySecFilter", 
  def = function(x, ...) standardGeneric("arraySecFilter")
)


#' @rdname arraySecFilter-methods
#' @aliases arraySecFilter
setMethod(
  f = "arraySecFilter",
  signature = "MultiSet",
  definition = function(x, control.arrays, remove.control.arrays = FALSE, plot = FALSE, ...){
    
    if(is.numeric(control.arrays)){
      nctrl <- x[ ,control.arrays]
      nctrl <- arrayBGcorr(nctrl, transform = "log2", method = "none")
      nctrl <- arraySummary(nctrl, method = "median")
    } else {
      stop("control.arrays must be a numeric value corresponding to the negative control array.")
    }
    
    sec.gmm <- normalmixEM(exprs(nctrl), k=2, maxit=100, epsilon=0.001)
    cutoff <- qnorm(0.95, sec.gmm$mu[1], sec.gmm$sig[1])
    secbinder <- nctrl[exprs(nctrl) > cutoff, ]
    x <- x[-which(fData(x)$ID %in% featureNames(secbinder)), ]
    
    if(remove.control.arrays){
      x <- x[ ,-control.arrays]
    }
    
    if(plot){
    plot(hist(exprs(nctrl), breaks = 100, col = "black"), ...)  
    curve(sec.gmm$lambda[1] * dnorm(x,mean = sec.gmm$mu[1], sd = sec.gmm$sigma[1]), col = "red")
    curve(sec.gmm$lambda[2] * dnorm(x,mean = sec.gmm$mu[2], sd = sec.gmm$sigma[2]), col = "blue", add=TRUE)
    }
    return(x)
    
  }
)