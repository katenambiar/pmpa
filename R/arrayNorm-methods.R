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
#' @exportMethod arrayNorm
#' @docType methods
#' @rdname arrayNorm-methods
setGeneric(
  name = "arrayNorm", 
  def = function(x, ...) standardGeneric("arrayNorm")
)


#' @rdname arrayNorm-methods
#' @aliases arrayNorm
setMethod(
  f = "arrayNorm",
  signature = "MultiSet",
  definition = function(x, method = "none", ...){
    
    if (method == "none"){
      
      return(x)
    
    } else if(method == "scale"){

      x <- scaleNorm(x, ...)
      return(x)

    } else if(method == "quantile"){
      
      assayDataElement(x, "fMedian") <- normalize.quantiles(fg(x))
      return(x)
    
    } else if (method == "LM"){
      
      assayDataElement(x, "fMedian") <- lmNorm(x, ...)
      return (x)
        
    } 
    
      stop("Method must be either 'scale', 'quantile', 'LM' or 'none'")
    
    }
)

#' @rdname arrayNorm-methods
#' @aliases arrayNorm
setMethod(
  f = "arrayNorm",
  signature = "ExpressionSet",
  definition = function(x, method = "none", skip.array = NULL, ...){
    
    fnames <- featureNames(x)  
    
    if (method == "none"){
      
      return(x)
      
    } else if(method == "scale"){
      
      exprs(x) <- scaleNorm(exprs(x))
      return(x)
      
    } else if(method == "scaleGMM"){
      
      exprs(x) <- scaleNormGMM(exprs(x))
      return(x)
      
    } else if(method == "quantile"){
      
      if(is.null(skip.array)){
        exprs(x) <- normalize.quantiles(exprs(x))
        featureNames(x) <- fnames
        return(x)
        }
    } 
    
    stop("Method must be either 'scale', 'quantile', or 'none'")
    
  }
)
