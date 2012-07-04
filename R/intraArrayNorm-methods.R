#-----------------------------------------------------------------------------------------------------
#   Intra Array Normalisation
#-----------------------------------------------------------------------------------------------------

setMethod(
  f = "intraArrayNorm",
  signature = "MultiSet",
  definition = function(x, controlSeq, method = "lm"){
    normdata <- NULL
    for (i in seq(1:ncol(x))){
      modeldata <- data.frame(id = fData(x)$ID,
                              intensity = log2(assayData(x)$fg)[,i],
                              printtip = factor(fData(x)$Block)
                              )
    
    # Subset model data to control seqs
    modeldata.control <- modeldata[modeldata$id %in% controlSeq,]
    
    # Fit linear model
    if (method == "lm"){
      fit <- lm (intensity ~ id + printtip, data = modeldata.control)
      } else stop("Method must be either lm, rlm or pls") 
    
      # Norm Matrix
      norm.matrix <- matrix(rep(0, length(modeldata$intensity) * length(coef(fit))), 
                            nrow = length(modeldata$intensity), 
                            ncol = length(coef(fit))
                            )
    
      for (i in 2:length(unique(modeldata$printtip))) {
        printtip <- which(modeldata$printtip == i)
        norm.matrix[printtip, (length(controlSeq) - 1 + i)] <- 1
      }
    
      norm.matrix <- norm.matrix %*% coef(fit)
    
      # Normalisation
      normdata.tmp <- modeldata$intensity - norm.matrix
      normdata <- cbind(normdata, normdata.tmp)
    }
  
  
    return (normdata)
    }
)