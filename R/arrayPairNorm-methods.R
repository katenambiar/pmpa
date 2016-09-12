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
#' @exportMethod arrayPairNorm
#' @docType methods
#' @rdname arrayPairNorm-methods
setGeneric(
  name = "arrayPairNorm", 
  def = function(x, ...) standardGeneric("arrayPairNorm")
)


#' @rdname arrayPairNorm-methods
#' @aliases arrayPairNorm
setMethod(
  f = "arrayPairNorm",
  signature = "MultiSet",
  definition = function(x, y, method = "none", ...){
 
  }
)

#' @rdname arrayPairNorm-methods
#' @aliases arrayPairNorm
setMethod(
  f = "arrayPairNorm",
  signature = "ExpressionSet",
  definition = function(x, y, method = "none", ...){
    
  }
)