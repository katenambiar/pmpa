# Calculate the mean of intra-array replicates with flagged values excluded
# Dependencies: 
# Arguments: x = matrix of intensity values with probes as rows and samples in columns
#            ndups = number of technical replicates on the microarray
#            spacing = number of rows separating a probe from its replicate
# Output: matrix of mean intensity values with samples in colums and unique probes in rows
# Last Updated: 3.2.2012

setMethod(
  f = "arrayAve",
  signature = "pepArrayPP",
  definition = function(x, ndups, spacing){
    
    y <- assayData(x)$fg
    y[assayData(x)$flags == 0] <- NA
    dim(y) <- c(spacing, ndups, ncol(y))
    ave <- apply(y, c(1,3), mean)
    se <- apply(y, c(1,3), function(z) sqrt(var(z)/length(z)))
    obj.tmp <- x[1:spacing,]
    
    obj <- new("pepArray")
    assayData(obj) <- assayDataNew(exprs = ave, exprs.se = se)
    phenoData(obj) <- phenoData(obj.tmp)
    featureData(obj) <- featureData(obj.tmp)
    phenoData(obj) <- phenoData(obj.tmp)
    protocolData(obj) <- protocolData(obj.tmp)
    return(obj)
    }
  )