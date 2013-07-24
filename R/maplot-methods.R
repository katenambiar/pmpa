#' MA-Plot
#' 
#' Creates simple MA plot
#' 
#' @param x MultiSet or ExpressionSet object
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
  signature = "MultiSet",
  definition = function(x, arr1, arr2, ...){
    y <- log2(fg(x))
    M <- y[ ,arr1] - y[ ,arr2]
    A <- (y[ ,arr1] + y[ ,arr2])/2
    plot(A, M,
         pch = 20,
         ...
         )
  }
)

setMethod(
  f = "maplot",
  signature = "ExpressionSet",
  definition = function(x, arr1, arr2, ...){
    y <- exprs(x)
    M <- y[ ,arr1] - y[ ,arr2]
    A <- (y[ ,arr1] + y[ ,arr2])/2
    plot(A, M,
         pch = 20,
         ...
    )
  }
)
