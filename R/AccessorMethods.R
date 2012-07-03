# Coefficient of Variation Accessor
setMethod(
  f = "cv",
  signature = "ExpressionSet",
  definition = function(x){
    assayDataElement(x, "cv.exprs")
  }
  )

# Foreground intensity accessor
setMethod(
  f = "fg",
  signature = "MultiSet",
  definition = function(x){
    assayDataElement(x, "fMedian")
  }
  )

# Background intensity accessor
setMethod(
  f = "bg",
  signature = "MultiSet",
  definition = function(x){
    assayDataElement(x, "bMedian")
  }
  )