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
    if (is.element("M", assayDataElementNames(x)) & is.element("A", assayDataElementNames(x))){
      M <- assayDataElement(x, "M")[ ,arrayID]
      A <- assayDataElement(x, "A")[ ,arrayID]
      plot(A, M, ...)
    }
    else {
      stop("M and A elements not found in ExpressionSet.")
    }
  }
)
