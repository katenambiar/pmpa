#' Boxplots of array blocks (print tip groups) from peptide microarray data
#' 
#' @param x MultiSet object with fMedian and/or bMedian matrices in assayData slot
#' @param arr Index indicating which array should be plotted
#' @param transform function to apply to transform the raw data
#' @return plot on current graphics device
#'  
#' @export
#' @docType methods
#' @rdname plotSubarrayBlocks-methods
setGeneric(
  name = "plotSubarrayBlocks", 
  def = function(x, ...) standardGeneric("plotSubarrayBlocks")
)

#' @rdname plotSubarrayBlocks-methods
#' @aliases plotSubarrayBlocks
setMethod(
  f = "plotSubarrayBlocks",
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
    plotdata <- data.frame(block = fData(x)$Block, 
                           subarray = fData(x)$Subarray, 
                           arraydata
                           )

    boxplot(arraydata ~ block, data = plotdata,
            col = rep(2:4, each = max(fData(x)$Block)/max(fData(x)$Subarray)),
            las = 1,
            pch = 20,
            xlab = "Block",
            ylab = "Signal Intensity", ...
    )
  }
)
