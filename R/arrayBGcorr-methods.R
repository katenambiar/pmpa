#' Local background feature correction algorithms
#' 
#' Implements local background correction for peptide microarray data
#' 
#' @param x MultiSet object with fMedian and bMedian matrices in the assayData slot
#' @param method One of 'none', 'subtract', 'ratio' or 'normexp'
#' @param transform Expression to transform raw data. Defaults to log2
#' @return MultiSet object with transformed and background corrected foreground signal in the fMedian matrix
#'  
#' @export
#' @docType methods
#' @rdname arrayBGcorr-methods
setGeneric(
  name = "arrayBGcorr", 
  def = function(x, ...) standardGeneric("arrayBGcorr")
)


#' @rdname arrayBGcorr-methods
#' @aliases arrayBGcorr
setMethod(
  f = "arrayBGcorr",
  signature = "MultiSet",
  definition = function(x, method, offset = 0, transform = "log2"){
    
    if (transform == "none"){
      
      if (method == "none"){
        
        return (x)
      } else if (method == "subtract"){
        
        assayDataElement(x, "fMedian") <- assayDataElement(x, "fMedian") - assayDataElement(x, "bMedian")
        minval <- min(assayDataElement(x, "fMedian")[assayDataElement(x, "fMedian") > 0])
        assayDataElement(x, "fMedian")[assayDataElement(x, "fMedian") <= 0] <- minval
        return(x)
        
      } else if (method == "ratio"){
        
        assayDataElement(x, "fMedian") <- assayDataElement(x, "fMedian") / assayDataElement(x, "bMedian")
        return(x)
        
      } else if (method == "normexp"){
        
        assayDataElement(x, "fMedian") <- backgroundCorrect.matrix(E = assayDataElement(x, "fMedian"), Eb = assayDataElement(x, "bMedian"), method = "normexp")
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
      
        assayDataElement(x, "fMedian") <- (assayDataElement(x, "fMedian") + offset) - assayDataElement(x, "bMedian")
        minval <- min(assayDataElement(x, "fMedian")[assayDataElement(x, "fMedian") > 0])
        assayDataElement(x, "fMedian")[assayDataElement(x, "fMedian") <= 0] <- minval
        assayDataElement(x, "fMedian") <- transformFunc(assayDataElement(x, "fMedian"))
        return(x)
      
      } else if (method == "ratio"){
      
        
        assayDataElement(x, "fMedian") <- transformFunc(assayDataElement(x, "fMedian") - assayDataElement(x, "bMedian"))
        return(x)
      
      } else if (method == "normexp"){
        
        assayDataElement(x, "fMedian") <- backgroundCorrect.matrix(E = assayDataElement(x, "fMedian"), Eb = assayDataElement(x, "bMedian"), method = "normexp")
        assayDataElement(x, "fMedian") <- transformFunc(assayDataElement(x, "fMedian"))
        return(x)
      
      } else {
      
        stop("Method must be either 'none', 'subtract' or 'ratio'")
      }
    }
  }
  )