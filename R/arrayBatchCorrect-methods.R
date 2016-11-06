#' Batch Correction for peptide array data 
#' (using ComBat algorithm)
#' 
#' Wrapper for the ComBat function within the sva package
#' 
#' @param x MultiSet, ExpressionSet or matrix object
#' @param batch vector indicating batch
#' @param mod model matrix for outcome of interest 
#' other than the batch
#' @return MultiSet, ExpressionSet or matrix object 
#' with batch corrected intensities
#'  
#' @import sva
#' @exportMethod arrayBatchCorrect
#' @docType methods
#' @rdname arrayBatchCorrect-methods
setGeneric(
  name = "arrayBatchCorrect", 
  def = function(x, ...) standardGeneric("arrayBatchCorrect")
)


#' @rdname arrayBatchCorrect-methods
#' @aliases arrayBatchCorrect
setMethod(
  f = "arrayBatchCorrect",
  signature = "MultiSet",
  definition = function(x, batch, mod, ...) {
    fg(x) <- ComBat(fg(x), batch, mod, ...)
    return(x)
  }
)

#' @rdname arrayBatchCorrect-methods
#' @aliases arrayBatchCorrect
setMethod(
  f = "arrayBatchCorrect",
  signature = "ExpressionSet",
  definition = function(x, batch, mod, ...) {
    exprs(x) <- ComBat(exprs(x), batch, mod, ...)
    return(x)
  }
)

#' @rdname arrayBatchCorrect-methods
#' @aliases arrayBatchCorrect
setMethod(
  f = "arrayBatchCorrect",
  signature = "matrix",
  definition = function(x, batch, mod, ...) {
    x <- ComBat(x, batch, mod, ...)
    return(x)
  }
)

