#' Filter Secondary Antibody Binding Peptide Probes
#' 
#' Implements filtering of signal from peptides that specifically bind to the secondary antibody.
#' Using a control array incubated just with the secondary antibody, significantly binding probes are
#' identified by fitting the data to a two-component Gaussian general mixture model. A cutoff signal intensity
#' is set at the 95th percentile of the distribution from the non-binding peptides. Any probes showing signal greater
#' than the cutoff are excluded.  
#' 
#' @param x MultiSet object after background correction and log transformation
#' @param control.arrays Numeric vector specifying negative control (secondary antibody only) arrays. 
#' @param removecontrol.arrays either TRUE to remove the control array after filtering or FALSE to leave the control array in the dataset. (default = TRUE)
#' @param plot Either TRUE to return a histogram with overlying density plots of the two mixture components on the current graphics device, 
#' or FALSE to implement the filtration without returning a plot. (default = FALES)
#' @param ... further arguments passed to \link{hist}
#' @return MultiSet object filtered to remove probes with high secondary binding
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
  definition = function(x, control.arrays, remove.control.arrays = TRUE, remove.probes = FALSE, plot = FALSE, ...){
    
    if(is.numeric(control.arrays)){
      nctrl <- x[ ,control.arrays]
      nctrl <- arrayBGcorr(nctrl, method = "none", transform = "log2")
      nctrl <- arraySummary(nctrl, method = "median")
    } else {
      stop("control.arrays must be a numeric value corresponding to the negative control array.")
    }
    
    sec.gmm <- normalmixEM(exprs(nctrl), k=2, maxit=100, epsilon=0.001)
    cutoff <- qnorm(0.95, sec.gmm$mu[1], sec.gmm$sig[1])
    secbinder <- nctrl[exprs(nctrl) > cutoff, ]
    fData(x)$secbinder <- FALSE
    fData(x)$secbinder[which(fData(x)$ID %in% featureNames(secbinder))] <- TRUE
    
    if(remove.probes){
      x <- x[-which(fData(x)$ID %in% featureNames(secbinder)), ]
    }
    
    if(remove.control.arrays){
      x <- x[ ,-control.arrays]
    }
    
    if(plot){
    hist(exprs(nctrl), breaks = 100, freq=FALSE, col = "dark grey")  
    curve(sec.gmm$lambda[1] * dnorm(x,mean = sec.gmm$mu[1], sd = sec.gmm$sigma[1]), col = "green", add = TRUE)
    curve(sec.gmm$lambda[2] * dnorm(x,mean = sec.gmm$mu[2], sd = sec.gmm$sigma[2]), col = "red", add = TRUE)
    abline(v = cutoff, col = "red")
    text(x = cutoff, y = 1, labels = "Cutoff")
    }
    return(x)
    
  }
)