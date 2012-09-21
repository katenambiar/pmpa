# Summarises intra-array replicates with flagged values excluded
# Dependencies: 
# Arguments: x = MultiSet object
#            method = c("TukeyBiweight", "MedianPolish", "Mean")
#
# Output: ExpressionSet with summarised intensity values in exprs slot
# Last Updated: 2.8.2012

setMethod(
  f = "arraySummary",
  signature = "MultiSet",
  definition = function(x, method, ...){
    
    if (!is.null(fData(x)$ID)){
      ID <- fData(x)$ID
      ID <- factor(ID, levels = unique(ID))
      
    } else {
      stop("Peptide IDs not defined")
      
    }
  
    y <- assayDataElement(x, "fMedian")
    flags <- assayDataElement(x, "flags")
    y[flags < -99] <- NA
    
    if (method == "mean"){
      arraySumm <- subColSummarizeAvg(y, ID)
      
    } else if (method == "median"){
      arraySumm <- subColSummarizeMedian(y, ID)
      
    } else if (method == "tukeybiweight"){
      arraySumm <- subColSummarizeBiweight(y, ID)
    
    } else if(method == "medianpolish"){
      arraySumm <- subColSummarizeMedianpolish(y, ID)
    } 
    
    obj <- new("ExpressionSet")
    assayData(obj) <- assayDataNew(exprs = arraySumm)
    phenoData(obj) <- phenoData(x)
    fData(obj) <- fData(x)[!duplicated(fData(x)$ID), ]
    featureNames(obj) <- fData(obj)$ID
    fData(obj) <- fData(obj)[ ,!(names(fData(obj)) %in% c("ID", "Block", "Column", "Row")), drop = FALSE]                     
    experimentData(obj) <- experimentData(x)
    protocolData(obj) <- protocolData(x)
                             
    return(obj)
    }
  )
