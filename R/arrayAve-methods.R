# Calculate the mean of intra-array replicates with flagged values excluded
# Dependencies: 
# Arguments: x = peparrayPP object
#
# Output: ExpressionSet with mean intensity values in exprs slot and CV in cv.exprs slot
# Last Updated: 11.6.2012

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
  
    y <- assayDataElement(x, "fMedian")
    flags <- assayDataElement(x, "flags")
    y[flags < -99] <- NA
    sum.y <- rowsum(y, ID, reorder = FALSE, na.rm = TRUE)
    sumsq.y <- rowsum(y^2, ID, reorder = FALSE, na.rm = TRUE)
    n <- rowsum(1L - is.na(y), ID, reorder = FALSE)
    ave <- sum.y/n
    
    var.y <- abs(((n * sumsq.y) - sum.y^2)/n^2)
    cv <- sqrt(var.y) / ave
    
    obj <- new("ExpressionSet")
    assayData(obj) <- assayDataNew(exprs = ave, cv.exprs = cv, nreps = n)
    phenoData(obj) <- phenoData(x)
    fData(obj) <- fData(x)[!duplicated(fData(x)$ID), ]
    featureNames(obj) <- fData(obj)$ID
    fData(obj) <- fData(obj)[ ,!(names(fData(obj)) %in% c("ID", "Block", "Column", "Row")), drop = FALSE]                     
    experimentData(obj) <- experimentData(x)
    protocolData(obj) <- protocolData(x)
                             
    return(obj)
    }
  )
