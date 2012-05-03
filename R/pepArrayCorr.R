#-----------------------------------------------------------------------------------------------------
#   Definitions for pepArrayCorr class
#-----------------------------------------------------------------------------------------------------

# pepArrayNorm Class Definition
setClass ("pepArrayCorr", 
          representation (I = "matrix", flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )

#-----------------------------------------------------------------------------------------------------
#   Accessor Methods for pepArrayCorr class
#-----------------------------------------------------------------------------------------------------

setMethod("getI", "pepArrayCorr", function(x) slot(x, "I"))
setMethod("getFlags", "pepArrayCorr", function(x) slot(x, "flags"))
setMethod("getPepAnnot", "pepArrayCorr", function(x) slot(x, "peptideAnnotation"))
setMethod("getSampAnnot", "pepArrayCorr", function(x) slot(x, "sampleAnnotation"))



pepArrayAve <- function(x, ndups, spacing, weights = NULL){
  # Calculate the mean of intra-array replicates with flagged values excluded
  # Dependencies: 
  # Arguments: x = matrix of intensity values with probes as rows and samples in columns
  #            ndups = number of technical replicates on the microarray
  #            spacing = number of rows separating a probe from its replicate
  # Output: matrix of mean intensity values with samples in colums and unique probes in rows
  # Last Updated: 3.2.2012
  
  x <- as.matrix(x)
  x[weights == 0] <- NA
  dim(x) <- c(spacing, ndups, ncol(x))
  ave <- apply(x, c(1,3), mean)
  return(ave)
}