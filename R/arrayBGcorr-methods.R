#' Local background feature correction algorithms
#' 
#' Implements local background correction for peptide microarray data
#' 
#' @param x MultiSet object with fMedian and 
#' bMedian matrices in the assayData slot
#' @param method Character string specifying background correction method. 
#' Valid methods are 'none', 'subtract', 'edwards', ratio' or 'normexp'. 
#' Defaults to 'none' if no method is specified.
#' @param offset numeric value added to raw signal intensity 
#' before background correction is implemented
#' @param transform Expression to transform raw data. Defaults to log2
#' @return MultiSet object with transformed and 
#' background corrected foreground signal in the fMedian matrix
#'  
#' @import limma
#' @exportMethod arrayBGcorr
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
  definition = function(x, method = "none", offset = 1, transform = "log2", ...){
    
    if (offset < 0){
      stop("Offset value must be positive")
    }
    
    if (is.function(transform)){
      transformFunc <- transform
      
    } else if (transform == "none"){
      transformFunc <- function(y) identity(y)
    
    } else {
      transformExpression <- parse(text = paste(transform, "(y)", sep = ""))
      transformFunc <- function (y){
        eval(transformExpression)
      }
    }
    
    if (method == "none"){
      assayDataElement(x, "fMedian") <- 
        transformFunc(assayDataElement(x, "fMedian"))
      return (x)
      
      
    } else if (method == "subtract"){
      assayDataElement(x, "fMedian") <- 
        (assayDataElement(x, "fMedian") + offset) - assayDataElement(x, "bMedian")
      assayDataElement(x, "fMedian")[assayDataElement(x, "fMedian") <= 0] <- offset
      assayDataElement(x, "fMedian") <- transformFunc(assayDataElement(x, "fMedian"))
      return(x)
      
      
    } else if (method == "ratio"){
      assayDataElement(x, "fMedian") <- transformFunc(assayDataElement(x, "fMedian"))
      - transformFunc(assayDataElement(x, "bMedian"))
      return(x)
      
      
    } else if (method == "edwards"){
      assayDataElement(x, "fMedian") <- backgroundCorrect.matrix(E = assayDataElement(x,
        "fMedian"), Eb = assayDataElement(x, "bMedian"), method = "edwards", ...)
      assayDataElement(x, "fMedian") <- transformFunc(assayDataElement(x, "fMedian"))
      return(x)  
      
      
    } else if (method == "normexp"){
      assayDataElement(x, "fMedian") <- 
        backgroundCorrect.matrix(E = assayDataElement(x, "fMedian"), 
                                 Eb = assayDataElement(x, "bMedian"), 
                                 method = "normexp", offset = offset, ...)
      assayDataElement(x, "fMedian") <- 
        transformFunc(assayDataElement(x, "fMedian"))
      return(x)
      
      
    } else {
      
      stop("Method must be either 'none', 'subtract', 'ratio', 'edwards' or 'normexp'")
    }
  }
  )