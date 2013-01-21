setMethod(
  f = "getArrayLayout",
  signature = "eSet",
  definition = function(x, ncols = 4, subarrays = 3){
    
    if(!all(c("Block", "Row", "Column") %in% names(fData(x))))
      stop("Block, Row and Column indexing not defined for the arrays")
    
    block.row <- ((fData(x)$Block - 1) %/% ncols) + 1
    block.col <- ((fData(x)$Block - 1) %% ncols) + 1
    spot.row <- fData(x)$Row
    spot.col <- fData(x)$Column
    subarray <- ((block.row - 1) %/% (max(block.row)/subarrays)) + 1
    
  }
    
)
