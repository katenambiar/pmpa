setMethod(
  f = "cv.exprs",
  signature = "ExpressionSet",
  definition = function(object){
    assayDataElement(object, "cv.exprs")
  }
  )

setMethod(
  f = "fg",
  signature = "pepArrayPP",
  definition = function(x){
    assayDataElement(x, "fMedian")
  }
  )

setMethod(
  f = "bg",
  signature = "pepArrayPP",
  definition = function(x){
    assayDataElement(x, "bMedian")
  }
  )