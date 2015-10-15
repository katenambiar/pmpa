#' Within array scaling normalisation of blocks (print-tip groups)
#' 
#' @param x MultiSet object with fMedian matrices in the assayData slot
#' @param arr integer value corresponding to array to be normalised
#' @return MultiSet object with normalised foreground signal 
#' in the fMedian matrix
#'  
#' @exportMethod blockScaleNorm
#' @docType methods
#' @rdname blockScaleNorm-methods
setGeneric(
  name = "blockScaleNorm", 
  def = function(x, ...) standardGeneric("blockScaleNorm")
)

#' @rdname blockScaleNorm-methods
#' @aliases blockScaleNorm
setMethod(
  f = "blockScaleNorm",
  signature = "MultiSet",
  definition = function(x){
    
    n.blocks <- max(fData(x)$Block)
    
    for (i in 1:ncol(x)){
      y <- matrix(fg(x)[,i], ncol = n.blocks)
      z <- .medianScaleMatrix(y)
      assayDataElement(x, "fMedian")[,i] <- c(z)
    }
    return(x)
  }
)

#--------------------------------------------------------------------------
#' Scale columns of a matrix to a common median (INTERNAL FUNCTION)
#' @keywords internal
.medianScaleMatrix <- function(x){
  y <- apply(x, 2, median, na.rm = TRUE)
  y <- y - exp(mean(log(abs(y))))
  z <- sweep(x, 2, y)
  return(z)
}
