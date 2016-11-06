#' Linear Model Normalisation
#' 
#' This function normalises microarray data by fitting a 
#' linear model and then using the residuals of the fit 
#' as the normalised data. It is normally called
#' from arrayNorm().
#' 
#' @param x MultiSet Object
#' @param sampleID vector of sample unique identifiers
#' @param featureID vector of feature identifiers 
#' (can include repeated features)
#' @param controlID vector of control feature identifiers 
#' (default = NULL)
#' @param include.subarrays boolean - to use subarrays 
#' as a factor in the linear model (default = TRUE)
#' @return matrix of normalised intensities
#' @export
lmNorm <- function(x, sampleID = sampleNames(x), featureID = fData(x)$ID, 
                   controlID = NULL, include.subarrays = TRUE){
  
  if(include.subarrays == TRUE){
    
    if(!is.null(controlID)){
      
      # LM Normalisation for control probes using subarrays
      arraylayout <- as.data.frame(getArrayLayout(x))
      ctrldata <- x[featureID %in% controlID, ]
      if(nrow(ctrldata) == 0){
        stop("Control IDs not found in data to be normalised")
      }
      ctrllayout <- as.data.frame(getArrayLayout(ctrldata))
      modeldata <- data.frame(Intensity = as.numeric(fg(ctrldata)), 
                              Sample = factor(rep(sampleID, each = nrow(ctrldata)), 
                                              levels = unique(sampleID)),
                              Subarray = factor(rep(ctrllayout$subarray, 
                                                    times = ncol(x)))
      )
      fit <- lm(Intensity ~ Sample:Subarray, data = modeldata)
      norm <- matrix(coef(fit)[-1], 
                     nrow = max(ctrllayout$subarray),  
                     byrow = TRUE
      )
      normmat <- NULL
      for (i in 1: max(ctrllayout$subarray)){
        normmat <- rbind(normmat, 
                         matrix(rep(norm[i, ], 
                                    times = sum(arraylayout$subarray == i)), 
                                ncol = ncol(x), 
                                byrow = TRUE
                                )
                         )
      } 
      normmat[is.na(normmat)] <- 0
      normdata <- fg(x) - normmat
      return (normdata)
      
    } else{
      
      # Global LM normalisation using subarrays
      arraylayout <- as.data.frame(getArrayLayout(x))
      modeldata <- data.frame(Intensity = as.numeric(fg(x)), 
                              Sample = factor(rep(sampleID, each = nrow(x)), 
                                              levels = unique(sampleID)),
                              Subarray = factor(rep(arraylayout$subarray, 
                                                    times = ncol(x)))
      )
      fit <- lm(Intensity ~ Sample:Subarray, data = modeldata)
      norm <- matrix(coef(fit)[-1], 
                     nrow = max(arraylayout$subarray),  
                     byrow = TRUE
      )
      normmat <- NULL
      for (i in 1: max(arraylayout$subarray)){
        normmat <- rbind(normmat, 
                         matrix(rep(norm[i, ], 
                                    times = sum(arraylayout$subarray == i)), 
                                ncol = ncol(x), byrow = TRUE)
        )
      }
      normmat[is.na(normmat)] <- 0
      normdata <- fg(x) - normmat
      return (normdata)
    }
    
  } else {
    
    if(!is.null(controlID)){
      # Control probe LM normalisation without subarray factor
      ctrldata <- x[featureID %in% controlID, ]
      if(nrow(ctrldata) == 0){
        stop("Control IDs not found in data to be normalised")
      }
      modeldata <- data.frame(Intensity = as.numeric(fg(ctrldata)), 
                              Sample = factor(rep(sampleID, each = nrow(ctrldata)), 
                                              levels = unique(sampleID))
      )
      fit <- lm(Intensity ~ Sample, data = modeldata)
      norm <- coef(fit)[-1]
      normmat <- matrix(rep(c(0, norm), times = nrow(x)), 
                        ncol = ncol(x), 
                        byrow = TRUE
                        )
      normdata <- fg(x) - normmat
      return (normdata)
      
    } else {
      
      # Global LM normalisation without subarray factor
      modeldata <- data.frame(Intensity = as.numeric(fg(x)), 
                              Sample = factor(rep(sampleID, each = nrow(x)), 
                                              levels = unique(sampleID))
      )
      fit <- lm(Intensity ~ Sample, data = modeldata)
      norm <- coef(fit)[-1]
      normmat <- matrix(rep(c(0, norm), times = nrow(x)), 
                        ncol = ncol(x), 
                        byrow = TRUE
                        )
      normdata <- fg(x) - normmat
      return (normdata)
    }
  }
}
