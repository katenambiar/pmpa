setMethod(
  f = "boxplot",
  signature = "pepArrayPP",
  definition = function(x, slot = "fMedian", transform = "none", ...){
    
    if (transform == "none"){
      plotdata <- assayDataElement(x, slot)
      colnames(plotdata) <- sampleNames(x)
    
    } else {
      transformExpression <- parse(text = paste(transform, "(y)", sep = ""))
      transformFunc <- function (y){
      eval(transformExpression)
    }
        
      plotdata <- assayDataElement(x, slot)
      plotdata <- transformFunc(plotdata)
      colnames(plotdata) <- sampleNames(x)
    }
    
    boxplot(plotdata, ...)

  }
  )

setMethod(
  f = "boxplot",
  signature = "pepArray",
  definition = function(x, transform = "none", ...){
    
    if (transform == "none"){
      plotdata <- assayDataElement(x, "exprs")
      colnames(plotdata) <- sampleNames(x)
      
    } else {
      transformExpression <- parse(text = paste(transform, "(y)", sep = ""))
      transformFunc <- function (y){
        eval(transformExpression)
      }
      
      plotdata <- assayDataElement(x, "exprs")
      plotdata <- transformFunc(plotdata)
      colnames(plotdata) <- sampleNames(x)
      
    }
    
    boxplot(plotdata, ...)
    
  }
  )