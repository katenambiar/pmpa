setGeneric(
  name = "arrayNorm", 
  def = function(x, ...) standardGeneric("arrayNorm")
)
setMethod(
  f = "arrayNorm",
  signature = "MultiSet",
  definition = function(x, method = "none", controlID = NULL){
    
    if (method == "none"){
      
      return(x)
    
    } else if(method == "scale"){

      assayDataElement(x, "fMedian") <- scaleNorm(fg(x))
      return(x)
      
    } else if(method == "quantile"){
      
      assayDataElement(x, "fMedian") <- normalize.quantiles(fg(x))
      return(x)
    
    } else if(method == "cyclic"){
      
      assayDataElement(x, "fMedian") <- normalizeCyclicLoess(fg(x), method = "affy")
      return(x)
        
    } else if (method == "LM"){
      
      assayDataElement(x, "fMedian") <- lmNorm(fg(x), sampleID = sampleNames(x), featureID = fData(x)$ID, controlID = controlID)
      return (x)
        
    } 
    
      stop("Method must be either 'scale', 'quantile', 'cyclic', 'LM' or 'none'")
    
    }
)