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
  definition = function(x, arrayID, ...){
    if(length(arrayID) != 2){
      stop("arrayID must be an integer vector of length 2 corresponding to the arrays to be plotted.")
    }
    y <- exprs(x)
    M <- y[ ,arrayID[1]] - y[ ,arrayID[2]]
    A <- (y[ ,arrayID[1]] + y[ ,arrayID[2]])/2
    plot(A, M, ...)
    lines(lowess(A,M), col = "blue")
    abline(h = 0, col = "red")
  }
)
