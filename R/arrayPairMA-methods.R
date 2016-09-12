#' Normalisation for peptide microarray data
#' 
#' @param x MultiSet object with fMedian matrix in the assayData slot
#' @param method Character string specifying normalisation method. 
#' Valid methods are 'none', 'scale', 'quantile' or 'LM'. 
#' Defaults to 'none' if no method is specified.
#' @param ... additional arguments passed to scaleNorm or lmNorm
#' @return MultiSet object with normalised signal in the fMedian matrix
#'  
#' @importFrom preprocessCore normalize.quantiles
#' @exportMethod arrayPairMA
#' @docType methods
#' @rdname arrayPairMA-methods
setGeneric(
  name = "arrayPairMA", 
  def = function(x, ...) standardGeneric("arrayPairMA")
)


#' @rdname arrayPairMA-methods
#' @aliases arrayPairMA
setMethod(
  f = "arrayPairMA",
  signature = "MultiSet",
  definition = function(x, p1, p2, method = "none", ...){
    if (length(p1) != length(p2)){
      stop("pair index vectors must be equal length")
    }
 
  }
)

#' @rdname arrayPairMA-methods
#' @aliases arrayPairMA
setMethod(
  f = "arrayPairMA",
  signature = "ExpressionSet",
  definition = function(x, p1, p2, method = "none", ...){
    
    if (length(p1) != length(p2)){
      stop("pair index vectors must be equal length")
    }
    
    M <- list()
    for (i in seq_along(p1)){
      M[[i]] <- exprs(x)[ ,p2[i]] - exprs(x)[ ,p1[i]]
    }
    
    M <- as.data.frame(M)
    
  }
)