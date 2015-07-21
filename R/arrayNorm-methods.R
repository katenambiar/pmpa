#' Normalisation
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
      
    } else if(method == "scaleGMM"){
      
      assayDataElement(x, "fMedian") <- scaleNormGMM(fg(x))
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
  definition = function(x, method = "none", ...){
    
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
      
      exprs(x) <- normalize.quantiles(exprs(x))
      featureNames(x) <- fnames
      return(x)
      
    } 
    
    stop("Method must be either 'scale', 'quantile', or 'none'")
    
  }
)
