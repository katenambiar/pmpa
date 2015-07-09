#' Scatter plots of subarrays from peptide microarray data
#' 
#' 
#' @param x MultiSet object with fMedian and/or bMedian matrices in the assayData slot
#' @param arr Index indicating which array should be plotted
#' @param subarray Vector of length = 2 indicating which subarrays should be plotted
#' @param transform function to apply to transform the raw data
#' @return plot on current graphics device
#'  
#' @export
#' @docType methods
#' @rdname plotSubarrays-methods
setGeneric(
  name = "plotSubarrays", 
  def = function(x, ...) standardGeneric("plotSubarrays")
)

#' @rdname plotSubarrays-methods
#' @aliases plotSubarrays
setMethod(
  f = "plotSubarrays",
  signature = "MultiSet",
  definition = function(x, arr, subarray = c(1,2), transform = "log2", ...){
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
    
    plotdata <- data.frame(SA1 = arraydata[fData(x)$Subarray == subarray[1]],
                           SA2 = arraydata[fData(x)$Subarray == subarray[2]])
    
    plot(SA1 ~ SA2, data = plotdata,
         las = 1,
         pch = 20,
         xlim = c(minval, maxval),
         ylim = c(minval, maxval),
         xlab = paste("SA", subarray[1]),
         ylab = paste("SA", subarray[2]),
         ...
    )
    lmfit <- lm(SA1 ~ SA2, data = plotdata,)
    abline(lmfit, col = "blue")
    abline(0,1, col = "red")
    
    lgnd1 <- bquote(R^2== .(round(summary(lmfit)$adj.r.squared, 3)))
    lgnd2 <- bquote(beta== .(round(coef(summary(lmfit))[2,1], 3)))
    legend("topleft", 
           c(as.expression(lgnd1),as.expression(lgnd2)),
           bty = "n",
           cex = 0.9
    )
  }
)





