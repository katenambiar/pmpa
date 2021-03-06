#' Create M and A values for each peptide array using a negative control array
#' 
#' Calculates M-values (log2 signal ratios) and A-values (log2 average signal) by comparison of
#' each peptide array with a negative control array.
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

    stopifnot(
      tryCatch(length(control.array) == 1, 
               error = function(err){
                 message("Only one control array can be used.")
               }
               )
      )

    tryCatch(
      {
        nctrl <- x[ ,control.array]
        assayDataElement(x, "M") <- exprs(x) - as.vector(exprs(nctrl))
        assayDataElement(x, "A") <- (exprs(x) + as.vector(exprs(nctrl)))/2
        return(x)
      },
      error = function(err){
        message(paste("Control array in column", control.array, "cannot be found."))
        message(err)
        return(NA)
      }
      )
  }
)