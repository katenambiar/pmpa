 #-----------------------------------------------------------------------------------------------------
#   Between Array Normalisation
#-----------------------------------------------------------------------------------------------------

setMethod(
  f = "normalizeArrays",
  signature = "MultiSet",
  definition = function(x, method = "none"){
    
    if (method == "none"){
      
      return(x)
      
    } else if(method == "quantile"){
      
      assayDataElement(x, "fMedian") <- normalize.quantiles(fg(x))
      return(x)
      
      } else if(method == "scale"){
        
        assayDataElement(x, "fMedian") <- normalizeBetweenArrays(fg(x), method = "scale")
        return(x)
      
      } else if(method == "cyclic"){
        
        assayDataElement(x, "fMedian") <- normalizeCyclicLoess(fg(x), method = "affy")
        return(x)
        
      } else if (method == "LM"){
        
        ndata <- fg(x)
        ndata <- as.numeric(ndata)
        ndata <- data.frame(Intensity = ndata, 
                            ID = rep(sampleNames(x), each = nrow(x)), 
                            SA = rep(fData(x)$Subarray, ncol(x))
                            )
        fit <- lm(Intensity ~ as.factor(ID) + as.factor(ID):as.factor(SA), data = ndata)
        normdata <- resid(fit) + coef(fit)[1]
        normdata <- matrix(normdata, ncol = ncol(x))
        assayDataElement(x, "fMedian") <- normdata
        return (x)
        
      } 
    
      stop("Method must be either 'scale', 'quantile', 'cyclic', 'LM' or 'none'")
    
    }
)