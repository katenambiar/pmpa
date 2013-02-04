setGeneric(
  name = "normaliseArrays", 
  def = function(x, ...) standardGeneric("normaliseArrays")
)
setMethod(
  f = "normaliseArrays",
  signature = "MultiSet",
  definition = function(x, method = "none", controlID = NULL){
    
    if (method == "none"){
      
      return(x)
    
    } else if(method == "scale"){
      
      ndata <- apply(fg(x), 2, median, na.rm = TRUE)
      ndata <- ndata - exp(mean(log(ndata)))
      assayDataElement(x, "fMedian") <- sweep(fg(x), 2, ndata)
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