#' Filter Secondary Antibody Binding Peptide Probes
#' 
#' Implements filtering of signal from peptides that specifically bind to the secondary antibody.
#' Using a control array incubated just with the secondary antibody, significantly binding probes are
#' identified by fitting the data to a two-component Gaussian general mixture model. The signal intensity
#' cutoff is set can be set at any point but defaults to the 95th percentile of the distribution from the 
#' non-binding peptides. Any probes showing signal greater than the cutoff are excluded.  
#' 
#' @param x MultiSet object after background correction and log transformation
#' @param control.arrays Numeric vector specifying negative control (secondary antibody only) arrays.
#' @param cutoff.quantile Percentile of the non-binding distributiion to set a cutoff (default = 0.95) 
#' @param bg.offset A positive constant to offset the foreground signal prior to subtractive background correction. 
#' Improves variance stabilisation for low intensity signal. See \link{arrayBGcorr}.
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
  definition = function(x, control.arrays, bg.offset = 1024, control.probes = NULL, cutoff.quantile = 0.95, remove.probes = TRUE, remove.control.arrays = TRUE){
    
    
    if(is.numeric(control.arrays)){
      nctrl <- x[ ,control.arrays]
      nctrl <- arrayBGcorr(nctrl, method = "subtract", offset = bg.offset, transform = "log2")
      nctrl <- arraySummary(nctrl, method = "median")
    } else {
      stop("control.arrays must be a numeric value or vector corresponding to the negative control array(s).")
    }
    
    sec.gmm <- list()
    cat("Fitting GMM to control array data... \n")
    for (i in 1:ncol(nctrl)){
      set.seed(123)
      sec.gmm[[i]] <- normalmixEM(exprs(nctrl)[,i], k = 2, maxit = 100, epsilon = 0.001, fast = TRUE)
      sec.gmm[[i]]$cutoff <- qnorm(cutoff.quantile, sec.gmm[[i]]$mu[1], sec.gmm[[i]]$sig[1])
      sec.gmm[[i]]$secbinder <- nctrl[exprs(nctrl)[,i] > sec.gmm[[i]]$cutoff, i]
    }
    
    common.secbinder <- Reduce(intersect, lapply(sec.gmm, function(x) featureNames(x$secbinder)))
    
    if(!is.null(control.probes)){
      common.secbinder <- common.secbinder[-which(common.secbinder %in% control.probes)]
    } 
    
    if(remove.probes){
      x <- x[-which(fData(x)$ID %in% common.secbinder), ]
      cat(length(common.secbinder), "unique secondary antibody binding probes found and removed.")
    } else {
      fData(x)$secbinder <- FALSE
      fData(x)$secbinder[which(fData(x)$ID %in% common.secbinder)] <- TRUE
      cat(length(common.secbinder), "unique secondary antibody binding probes found.")
    }
    
    if(remove.control.arrays){
      x <- x[ ,-control.arrays]
    }
    
    return(x)
    
  }
)

setMethod(
  f = "arraySecFilter",
  signature = "ExpressionSet",
  definition = function(x, control.arrays, control.probes = NULL, cutoff.quantile = 0.95, remove.probes = TRUE, remove.control.arrays = TRUE){
    
    if(is.numeric(control.arrays)){
      nctrl <- x[ ,control.arrays]
    } else {
      stop("control.arrays must be a numeric value or vector corresponding to the negative control array(s).")
    }
    
    sec.gmm <- list()
    cat("Fitting GMM to control array data... \n")
    for (i in 1:ncol(nctrl)){
      set.seed(123)
      sec.gmm[[i]] <- normalmixEM(exprs(nctrl)[,i], k = 2, maxit = 100, epsilon = 0.001, fast = TRUE)
      sec.gmm[[i]]$cutoff <- qnorm(cutoff.quantile, sec.gmm[[i]]$mu[1], sec.gmm[[i]]$sig[1])
      sec.gmm[[i]]$secbinder <- nctrl[exprs(nctrl)[,i] > sec.gmm[[i]]$cutoff, i]
    }
    
    common.secbinder <- Reduce(intersect, lapply(sec.gmm, function(x) featureNames(x$secbinder)))
    
    if(!is.null(control.probes)){
      common.secbinder <- common.secbinder[-which(common.secbinder %in% control.probes)]
    } 
    
    if(remove.probes){
      x <- x[-which(fData(x)$ID %in% common.secbinder), ]
      cat(length(common.secbinder), "unique secondary antibody binding probes found and removed.")
    } else {
      fData(x)$secbinder <- FALSE
      fData(x)$secbinder[which(fData(x)$ID %in% common.secbinder)] <- TRUE
      cat(length(common.secbinder), "unique secondary antibody binding probes found.")
    }
    
    if(remove.control.arrays){
      x <- x[ ,-control.arrays]
    }
    
    return(x)
    
  }
)
