setGeneric(
  name = "arrayNorm", 
  def = function(x, ...) standardGeneric("arrayNorm")
)
setMethod(
  f = "arrayNorm",
  signature = "MultiSet",
  definition = function(x, method = "none", ...){
    
    if (method == "none"){
      
      return(x)
    
    } else if(method == "scale"){

      assayDataElement(x, "fMedian") <- scaleNorm(fg(x))
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

setMethod(
  f = "arrayNorm",
  signature = "ExpressionSet",
  definition = function(x, method = "none", ...){
    
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
      return(x)
      
    } 
    
    stop("Method must be either 'scale', 'quantile', or 'none'")
    
  }
)
