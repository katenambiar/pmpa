#' Array Layout Accessor
#' 
#' Gets microarray layout information matrix
#' Used to obtain the layout information for \link{plotImage}
#' 
#' @param x MultiSet object with block, column and row information in the fData slot
#' @return layout matrix
#' @export
#' @docType methods
#' @rdname getArrayLayout-methods
setGeneric(
  name = "getArrayLayout",
  def = function(x, ...) standardGeneric("getArrayLayout")
)
#' @rdname getArrayLayout-methods
#' @aliases getArrayLayout
setMethod(
  f = "getArrayLayout",
  signature = "eSet",
  definition = function(x, ncols = 4, subarrays = 3){
    
    if(!all(c("Block", "Row", "Column") %in% names(fData(x))))
      stop("Feature data does not contain Block, Row and/or Column indexes")
    
    block.row <- ((fData(x)$Block - 1) %/% ncols) + 1
    block.col <- ((fData(x)$Block - 1) %% ncols) + 1
    spot.row <- fData(x)$Row
    spot.col <- fData(x)$Column
    subarray <- ((block.row - 1) %/% (max(block.row)/subarrays)) + 1
    
    y <- cbind(block.row, block.col, spot.row, spot.col, subarray)  
    
    return (y)
  }
    
)
