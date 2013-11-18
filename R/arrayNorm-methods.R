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