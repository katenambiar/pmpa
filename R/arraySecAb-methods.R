setMethod(
  f = "arrayCV",
  signature = "pepArrayPP",
  definition = function(x, secabID, transform = "none"){
  
    if (transform == "none"){
      secabdata <- assayDataElement(x[ , sampleNames(x) == secabID], "fg")
      arraydata <- assayDataElement(x[ , sampleNames(x) != secabID], "fg")
    
    } else {
      transformExpression <- parse(text = paste(transform, "(y)", sep = ""))
      transformFunc <- function (y){
        eval(transformExpression)
    }
    
      secabdata <- assayDataElement(x[ , sampleNames(x) == secabID], "fg")
      secabdata <- transformFunc(secabdata)
      arraydata <- assayDataElement(x[ , sampleNames(x) != secabID], "fg")
      arraydata <- transformFunc(arraydata)
    }
  
    fitlm <- apply(arraydata, 2, function (y) lm(y ~ secabdata))
    normdata <- sapply(fitlm, function(y) y$resid)
  
    z <- x[ , sampleNames(x) != secabID]
    assayDataElement(z, "fg") <- normdata
    return (z)
    }
  )
