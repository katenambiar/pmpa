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
  signature = "MultiSet",
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
    
    sum.y <- rowsum(y, ID, reorder = FALSE, na.rm = TRUE)
    sumsq.y <- rowsum(y^2, ID, reorder = FALSE, na.rm = TRUE)
    n <- rowsum(1L - is.na(y), ID, reorder = FALSE)
    ave <- sum.y/n
    var.y <- ((n * sumsq.y) - sum.y^2)/n^2
    cv <- sqrt(var.y) / ave
    
    return(cv)
    
  }
  )
