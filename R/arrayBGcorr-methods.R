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
# Last Updated: 30.4.2012
setMethod(
  f = "arrayBGcorr",
  signature = "pepArrayPP",
  definition = function(x, method){
    
    if (method == "none"){
      
      return (x)
    } else if (method == "subtract"){
      
      assayDataElement(x, fg) <- assayDataElement(x, fg) - assayDataElement(x, bg)
      return(x)
      
    } else {
      
      stop("Method must be either 'none', 'subtract' or 'SNR'")
    }
  }
  )