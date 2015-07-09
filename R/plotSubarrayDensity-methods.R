#' Density plots of subarrays from peptide microarray data
#' 
#' @param x MultiSet object with fMedian and/or bMedian matrices in the assayData slot
#' @param arr Index indicating which array should be plotted
#' @param subarray Vector of length = 2 indicating which subarrays should be plotted
#' @param transform function to apply to transform the raw data
#' @return plot on current graphics device
#'  
#' @export
#' @docType methods
#' @rdname plotSubarrayDensity-methods
setGeneric(
  name = "plotSubarrayDensity", 
  def = function(x, ...) standardGeneric("plotSubarrayDensity")
)

#' @rdname plotSubarrayDensity-methods
#' @aliases plotSubarrayDensity
setMethod(
  f = "plotSubarrayDensity",
  signature = "MultiSet",
  definition = function(x, arr, transform = "log2", ...){
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
    
    arraydata <- transformFunc(fg(x[ ,arr]))
    minval <- min(arraydata)
    maxval <- max(arraydata)
    
    plotdata <- data.frame(SA1 = arraydata[fData(x)$Subarray == 1],
                           SA2 = arraydata[fData(x)$Subarray == 2],
                           SA3 = arraydata[fData(x)$Subarray == 3]
                           )
    
    plot(density(plotdata$SA1),
         las = 1,
         xlim = c(minval, maxval),
         ...
    )
    lines(density(plotdata$SA2))
    lines(density(plotdata$SA3))
    
  }
)
