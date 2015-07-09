#' Scale Normalisation
#' 
#' 
#' @param x MultiSet object with fMedian matrices in the assayData slot
#' @param offset numeric value added to raw signal intensity before background correction is implemented
#' @return MultiSet object with normalised foreground signal in the fMedian matrix
#'  
#' @exportMethod scaleNorm
#' @docType methods
#' @rdname scaleNorm-methods
setGeneric(
  name = "scaleNorm", 
  def = function(x, ...) standardGeneric("scaleNorm")
)

#' @rdname scaleNorm-methods
#' @aliases scaleNorm
setMethod(
  f = "scaleNorm",
  signature = "MultiSet",
  definition = function(x, controlID = NULL){
    
    if (is.null(controlID)){
      y <- apply(fg(x), 2, median, na.rm = TRUE)
      y <- y - exp(mean(log(abs(y))))
      z <- sweep(fg(x), 2, y)
      assayDataElement(x, "fMedian") <- z
      
    } else {
      y <- fg(x)[fData(x)$ID %in% controlID, ]
      y <- apply(y, 2, median, na.rm = TRUE)
      y <- y - exp(mean(log(abs(y))))
      z <- sweep(fg(x), 2, y)
      assayDataElement(x, "fMedian") <- z
      
      return(x)
    }
    
  }
)


#' @rdname scaleNorm-methods
#' @aliases scaleNorm
setMethod(
  f = "scaleNorm",
  signature = "ExpressionSet",
  definition = function(x, controlID = NULL){
    
    if (is.null(controlID)){
      y <- apply(exprs(x), 2, median, na.rm = TRUE)
      y <- y - exp(mean(log(abs(y))))
      z <- sweep(exprs(x), 2, y)
      exprs(x) <- z
      
    } else {
      y <- exprs(x)[featureNames(x) %in% controlID, ]
      y <- apply(y, 2, median, na.rm = TRUE)
      y <- y - exp(mean(log(abs(y))))
      z <- sweep(exprs(x), 2, y)
      exprs(x) <- z
      
      return(x)
    }
    
  }
)


#' Scale Normalisation using GMM
#' 
#' @param x matrix of intensity values with 
#' probes as rows and samples in columns
#' @return matrix of normalised intensities
#' @export
scaleNormGMM <- function(x){
  
  model.gmm <- list()
  for (i in 1:ncol(x)){
    set.seed(123)
    model.gmm[[i]] <- normalmixEM(x[,i], k = 2, maxit = 100, 
                                  epsilon = 0.001, fast = TRUE)
  }
  
  mean.model.gmm <- sapply(model.gmm, function(x) x$mu[1])
  y <- mean.model.gmm - exp(mean(log(abs(mean.model.gmm))))
  z <- sweep(x, 2, y) 
  return(z)
}