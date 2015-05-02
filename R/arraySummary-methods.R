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
  definition = function(x, method, cv.threshold = 0.5, ...){
    
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
      arraySumm <- .meanID(y, ID)
      
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



#' Calculate the mean of intra-array replicates
#' @param x matrix of signal intensities with samples in columns and probes in rows
#' @param ID vector (factor) of identifiers for each probe corresponding to the rows in the x matrix
#' @return matrix of mean values with samples in columns and unique probes in rows
#' @keywords internal
.meanID <- function(x, ID){
  csum <- rowsum(x, group = ID, reorder = TRUE, na.rm = TRUE)
  n <- tabulate(ID)
  cmean <- csum/n
  return (cmean)
}

#' Calculate the CV of intra-array replicates
#' @param x matrix of signal intensities with samples in columns and probes in rows
#' @param ID vector (factor) of identifiers for each probe corresponding to the rows in the x matrix
#' @return matrix of CV values with samples in columns and unique probes in rows
#' @keywords internal
.cvID <- function(x, ID){
  csum <- rowsum(x, group = ID, reorder = TRUE, na.rm = TRUE)
  csumsq <- rowsum(x^2, group = ID, reorder = TRUE, na.rm = TRUE)
  n <- tabulate(ID)
  cmean <- csum/n
  cvar <- ((n * csumsq) - csum^2)/n^2
  cv <- sqrt(cvar) / cmean
  return(cv)
}




