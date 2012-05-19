#-----------------------------------------------------------------------------------------------------
#   ArrayCV Methods
#-----------------------------------------------------------------------------------------------------
#
#
# Calculate the CV of intra-array replicates
# Dependencies: 
# Arguments: x = PepArrayRaw object or matrix of intensity values with probes as rows and samples in columns
#            ndups = number of technical replicates on the microarray
#            spacing = number of rows separating a probe from its replicate
# Output:    matrix of CV values with samples in columns and unique probes in rows
#
# Kate Nambiar
# Last Updated: 30.4.2012
setMethod(
  f = "arrayCV",
  signature = "pepArrayPP",
  definition = function(x, transform = "log2"){
    
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
    sd.y <- aggregate(y, by = list(ID), FUN = "sd", simplify = TRUE)
    cv <- sd.y[,-1]/mean.y[,-1]
    rownames(cv) <- mean.y$Group.1
    return(cv)
    
  }
  )


setMethod(
  f = "arrayCV",
  signature = "matrix",
  definition = function(x, ndups, spacing){
    dim(x) <- c(spacing, ndups, ncol(x))
    cv <- apply(x, c(1,3), sd) / apply(x, c(1,3), mean)
    return(cv)
    
  }
  )