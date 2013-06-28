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
         pch = 20
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
         pch = 20
    )
  }
)
