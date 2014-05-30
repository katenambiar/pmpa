#' Create M and A values for each peptide array using a negative control array
#' 
#' Calculates M-values (log2 signal ratios) and A-values (log2 average signal) by comparison of
#' each peptide array with a negative control array that was incubated with only the secondary antibody.
#' 
#' @param x ExpressionSet object
#' @param control.array Integer value corresponding to the column index of the negative control array 
#' @return ExpressionSet object with M and A matrices added to the assayData environment
#'  
#' @exportMethod arrayControlMA
#' @docType methods
#' @rdname arrayControlMA-methods
setGeneric(
  name = "arrayControlMA", 
  def = function(x, ...) standardGeneric("arrayControlMA")
)


#' @rdname arrayControlMA-methods
#' @aliases arrayControlMA
setMethod(
  f = "arrayControlMA",
  signature = "ExpressionSet",
  definition = function(x, control.array){
    
    nctrl <- x[ ,control.array]
    assayDataElement(x, "M") <- exprs(x) - as.vector(exprs(nctrl))
    assayDataElement(x, "A") <- (exprs(x) + as.vector(exprs(nctrl)))/2
    return(x)
  }
)