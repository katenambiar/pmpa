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
    
    plotdata <- data.frame(SA1 = arraydata[fData(x)$Subarray == 1],
                           SA2 = arraydata[fData(x)$Subarray == 2],
                           SA3 = arraydata[fData(x)$Subarray == 3]
                           )
    plotdata <- apply(plotdata, 2, density)
    range.x <- lapply(plotdata, function(y) range(y$x))
    range.y <- lapply(plotdata, function(y) range(y$y))
    
    plot(0,
         type = "n",
         las = 1,
         xlim = range(range.x),
         ylim = c(0, range(range.y)[2]),
         xlab = "Signal Intensity",
         ylab = "Density",
         ...
    )
    lines(plotdata$SA1, col = "red")
    lines(plotdata$SA2, col = "green")
    lines(plotdata$SA3, col = "blue")
    
    legend("topright", 
           inset = 0,
           c("SA1", "SA2", "SA3"), 
           bty = "n", 
           fill = c(2:4), 
           cex = 0.5
           )

  }
)
