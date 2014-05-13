#' Summarises intra-array replicates with flagged values excluded
#'
#' @param x MultiSet object
#' @param method c("Median", "Mean")
#' @return ExpressionSet with summarised intensity values in exprs slot
#' @import preprocessCore
#' @exportMethod arraySummary
#' @docType methods arraySummary
#' @rdname arraySummary-methods
setGeneric(
  name = "arraySummary", 
  def = function(x, ...) standardGeneric("arraySummary")
)


#' @rdname arraySummary-methods
#' @aliases arraySummary
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
