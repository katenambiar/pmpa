#' MA-Plot
#' 
#' Creates simple MA plot
#' 
#' @param x ExpressionSet object
#' @param arr1 integer corresponding to 1st array to be plotted
#' @param arr2 integer corresponding to 2nd array to be plotted
#' @param ... other arguments passed to plot
#' @return Plot created on current graphics device
#'  
#' @export
#' @docType methods
#' @rdname maplot-methods
setGeneric(
  name = "maplot", 
  def = function(x, ...) standardGeneric("maplot")
)


setMethod(
  f = "maplot",
  signature = "ExpressionSet",
  definition = function(x, arrayID, labels = FALSE, nlabels = 1, label.names = featureNames(x), ...){
    if(length(arrayID) != 2){
      stop("arrayID must be an integer vector of length 2 corresponding to the arrays to be plotted.")
    }
    y <- exprs(x)
    M <- y[ ,arrayID[1]] - y[ ,arrayID[2]]
    A <- (y[ ,arrayID[1]] + y[ ,arrayID[2]])/2
    MAdata <- data.frame(M, A, row.names = label.names)
    MAdata <- MAdata[order(MAdata$M, decreasing = TRUE), ]
    
    plot(M ~ A, data = MAdata, ...)
    lines(lowess(A,M), col = "green")
    abline(h = 0, col = "red")
    
    
    text(M ~ A, data = MAdata[1:nlabels, ],
         labels = rownames(MAdata)[1:nlabels], 
         cex = 0.5, 
         pos = 4, 
         col = "red"
    )
  }
)
