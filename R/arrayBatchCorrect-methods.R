setGeneric(
  name = "arrayBatchCorrect", 
  def = function(x, ...) standardGeneric("arrayBatchCorrect")
)

setMethod(
  f = "arrayBatchCorrect",
  signature = "MultiSet",
  definition = function(x, batch, mod, ...) {
    fg(x) <- ComBat(fg(x), batch, mod, ...)
    return(x)
  }
)

setMethod(
  f = "arrayBatchCorrect",
  signature = "ExpressionSet",
  definition = function(x, batch, mod, ...) {
    exprs(x) <- ComBat(exprs(x), batch, mod, ...)
    return(x)
  }
)

setMethod(
  f = "arrayBatchCorrect",
  signature = "matrix",
  definition = function(x, batch, mod, ...) {
    x <- ComBat(x, batch, mod, ...)
    return(x)
  }
)

