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
  definition = function(x, ndups, spacing){

    y <- log2(assayData(x)$fg)
    y[assayData(x)$flags == 0] <- NA
    dim(y) <- c(spacing, ndups, ncol(y))
    cv <- apply(y, c(1,3), sd) / apply(y, c(1,3), mean)
    colnames(cv) <- sampleNames(x)
    return(cv)
    
  }
  )
