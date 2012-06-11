setMethod(
  f = "arrayReplicateQC",
  signature = "pepArrayPP",
  definition = function(x, transform = "none"){
    
    if (!is.null(fData(x)$ID)){
      ID <- fData(x)$ID
      ID <- factor(ID, levels = unique(ID))
      
    } else {
      stop("Peptide IDs not defined")
      
    }
    
    if (transform == "none"){
      y <- assayDataElement(x, "fg")
      colnames(y) <- sampleNames(x)
      
    } else {
      transformExpression <- parse(text = paste(transform, "(y)", sep = ""))
      transformFunc <- function (y){
        eval(transformExpression)
      }
      
      y <- assayDataElement(x, "fg")
      y <- transformFunc(y)
      colnames(y) <- sampleNames(x)
    }
    
    flags <- assayDataElement(x, "flags")
    y[flags == 0] <- NA
    
    mean.y <- aggregate(y, by = list(ID), FUN = "mean", simplify = TRUE)
    
  }
)
