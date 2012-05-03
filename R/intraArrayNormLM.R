#-----------------------------------------------------------------------------------------------------
#   Intra Array Normalisation
#-----------------------------------------------------------------------------------------------------

intraArrayNormLM <- function(x, controlSeq, method = "lm"){
  
  normdata <- NULL
  for (i in seq(1:ncol(fg(x)))){
    modeldata <- data.frame(id = peptideAnnotation(x)$ID,
                            intensity = log2(fg(x))[,i],
                            printtip = factor(peptideAnnotation(x)$Block)
                            )
    
    # Subset model data to control seqs
    modeldata.control <- modeldata[modeldata$id %in% controlSeq,]
    
    # Fit linear model
    if (method == "lm"){
      fit <- lm (intensity ~ id + printtip, data = modeldata.control)
      
    } else if (method == "rlm"){
      require(MASS)
      fit <- rlm (intensity ~ id + printtip, data = modeldata.control)
      
    } else if (method == "pls"){
      require(pls)
      fit <- plsr (intensity ~ id + printtip, data = modeldata.control)
      
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