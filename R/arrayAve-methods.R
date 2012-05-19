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
  definition = function(x){
    
    if (!is.null(fData(x)$ID)){
      ID <- fData(x)$ID
      ID <- factor(ID, levels = unique(ID))
      
    } else {
      stop("Peptide IDs not defined")
      
    }
  
    y <- assayDataElement(x, "fg")
    flags <- assayDataElement(x, "flags")
    y[flags == 0] <- NA
    sum.y <- rowsum(y, ID, reorder = FALSE, na.rm = TRUE)
    n <- rowsum(1L - is.na(y), ID, reorder = FALSE)
    ave <- sum.y/n
  
    obj <- new("pepArray")
    assayData(obj) <- assayDataNew(exprs = ave)
    phenoData(obj) <- phenoData(x)
    fData(obj) <- fData(x)[!duplicated(fData(x)$ID), ]
    featureNames(obj) <- fData(obj)$ID
    fData(obj) <- fData(obj)[ ,!(names(fData(obj)) %in% c("ID", "Block", "Column", "Row")), drop = FALSE]                     
    experimentData(obj) <- experimentData(x)
    protocolData(obj) <- protocolData(x)
                             
    return(obj)
    }
  )
