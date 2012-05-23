#-----------------------------------------------------------------------------------------------------
#   arrayBGcorr Methods
#-----------------------------------------------------------------------------------------------------
#
# Local background feature correction algorithms
# Arguments: x = PepArrayRaw object or matrix of intensity values with probes as rows and samples in columns
#            ndups = number of technical replicates on the microarray
#            spacing = number of rows separating a probe from its replicate
# Output:    matrix of CV values with samples in columns and unique probes in rows
#
# Kate Nambiar
# Last Updated: 23.5.2012
setMethod(
  f = "arrayBGcorr",
  signature = "pepArrayPP",
  definition = function(x, method, transform = "log2"){
    
    if (transform == "none"){
      
      if (method == "none"){
        
        return (x)
      } else if (method == "subtract"){
        
        assayDataElement(x, "fMedian") <- assayDataElement(x, "fMedian") - assayDataElement(x, "bMedian")
        return(x)
        
      } else if (method == "ratio"){
        
        assayDataElement(x, "fMedian") <- assayDataElement(x, "fMedian") / assayDataElement(x, "bMedian")
        return(x)
        
      } else {
        
        stop("Method must be either 'none', 'subtract' or 'ratio'")
      }
      
    } else {
      transformExpression <- parse(text = paste(transform, "(y)", sep = ""))
      transformFunc <- function (y){
        eval(transformExpression)
      }
    
      if (method == "none"){
      
        assayDataElement(x, "fMedian") <- transformFunc(assayDataElement(x, "fMedian"))
        return (x)
      } else if (method == "subtract"){
      
        assayDataElement(x, "fMedian") <- transformFunc(assayDataElement(x, "fMedian") - assayDataElement(x, "bMedian"))
        return(x)
      
      } else if (method == "ratio"){
      
        assayDataElement(x, "fMedian") <- transformFunc(assayDataElement(x, "fMedian") / assayDataElement(x, "bMedian"))
        return(x)
      
      } else {
      
        stop("Method must be either 'none', 'subtract' or 'ratio'")
      }
    }
  }
  )