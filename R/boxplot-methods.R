setMethod(
  f = "boxplot",
  signature = "pepArrayPP",
  definition = function(x, slot = "fg", transform = "log2", ...){
    
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
  definition = function(x, ...){
    
    plotdata <- assayDataElement(x, "exprs")
    colnames(plotdata) <- sampleNames(x)
    boxplot(plotdata, ...)
    
  }
  )