#' Summarises intra-array replicates with flagged values excluded
#'
#' @param x MultiSet object
#' @param method c("Median", "Mean")
#' @return ExpressionSet with summarised intensity values in exprs slot
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
  definition = function(x, method, cv.threshold){
    
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
      
    } else if (method == "mean.closest"){
      
      if(abs(max(table(fData(x)$Subarray)) - min(table(fData(x)$Subarray))) != 0){
        stop("Subarray lengths must be equal to use this method.")
      }
      
      ID.subarray <- fData(x)$ID[fData(x)$Subarray == 1]
      ID.subarray <- factor(ID.subarray, levels = unique(ID.subarray))
      
      z <- array(y, dim = c(table(fData(x)$Subarray)[1], 3, dim(x)[2]))
      z <- aperm(z, c(1,3,2))
      z.mean.pair <- apply(z, c(1,2), .meanClosestPair, cv.threshold)
      arraySumm <- .meanID(z.mean.pair, ID.subarray)
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


#' Calculate the mean of intra-array replicates (INTERNAL FUNCTION)
#' @keywords internal
.meanID <- function(x, ID){
  csum <- rowsum(x, group = ID, reorder = TRUE, na.rm = TRUE)
  n <- tabulate(ID)
  cmean <- csum/n
  return (cmean)
}

#' Calculate the CV of intra-array replicates (INTERNAL FUNCTION)
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

#' Mean of closest pair of subarray replicates if CV over threshold value (INTERNAL FUNCTION)
#' Only works for triple subarray microarrays
#' @keywords internal
.meanClosestPair <- function(x, cv.threshold ){
  x.mean <- mean(x, na.rm = TRUE)
  x.sd <- sd(x, na.rm = TRUE)
  x.cv <- x.sd/x.mean
  x.cv[is.na(x.cv)] <- 0
  
  if(x.cv >= cv.threshold){
    y.means <- c(mean(c(x[1], x[2]), na.rm = TRUE),
                 mean(c(x[1], x[3]), na.rm = TRUE),
                 mean(c(x[2], x[3]), na.rm = TRUE)
    )
    y.mean <- y.means[which.min(c(abs(x[1]-x[2]), abs(x[1]-x[3]), abs(x[2]-x[3])))]
    return(y.mean)
    
  } else{
    return (x.mean)
  }
}


