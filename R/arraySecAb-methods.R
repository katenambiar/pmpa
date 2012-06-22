setMethod(
  f = "arraySecAb",
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
    normdata <- sapply(fitlm, function(y) y$resid + y$coef[1])
  
    z <- x[ , sampleNames(x) != secabID]
    assayDataElement(z, "fg") <- normdata
    return (z)
    }
  )


setMethod(
  f = "arraySecAb",
  signature = "pepArray",
  definition = function(x, secabID, transform = "none", method = "lm"){
    
    if (transform == "none"){
      secabdata <- assayDataElement(x[ , sampleNames(x) == secabID], "exprs")
      arraydata <- assayDataElement(x[ , sampleNames(x) != secabID], "exprs")
      
    } else {
      transformExpression <- parse(text = paste(transform, "(y)", sep = ""))
      transformFunc <- function (y){
        eval(transformExpression)
      }
      
      secabdata <- assayDataElement(x[ , sampleNames(x) == secabID], "exprs")
      secabdata <- transformFunc(secabdata)
      arraydata <- assayDataElement(x[ , sampleNames(x) != secabID], "exprs")
      arraydata <- transformFunc(arraydata)
    }
    
    if (method == "lm"){
      fitlm <- apply(arraydata, 2, function (y) lm(y ~ secabdata))
      
    } else if (method == "rlm"){
      fitlm <- apply(arraydata, 2, function (y) rlm(y ~ secabdata))
      
    } else {
      stop("Method must be either 'lm' or 'rlm'")
      
    }
    
      normdata <- sapply(fitlm, function(y) y$resid + y$coef[1])
    
    z <- x[ , sampleNames(x) != secabID]
    assayDataElement(z, "exprs") <- normdata
    return (z)
  }
  )
