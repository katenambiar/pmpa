setMethod(
  f = "exprs",
  signature = "pepArray",
  definition = function(object){
    assayDataElement(object, "exprs")
  }
  )

setMethod(
  f = "fg",
  signature = "pepArrayPP",
  definition = function(x){
    assayDataElement(x, "fg")
  }
  )

setMethod(
  f = "bg",
  signature = "pepArrayPP",
  definition = function(x){
    assayDataElement(x, "bg")
  }
  )