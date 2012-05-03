#-----------------------------------------------------------------------------------------------------
#   Definitions for pepArrayNorm class
#-----------------------------------------------------------------------------------------------------

# pepArrayNorm Class Definition
setClass ("pepArrayNorm", 
          representation (I = "matrix", flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )

#-----------------------------------------------------------------------------------------------------
#   Accessor Methods for pepArrayNorm class
#-----------------------------------------------------------------------------------------------------

setMethod("getI", "pepArrayNorm", function(x) slot(x, "I"))
setMethod("getFlags", "pepArrayNorm", function(x) slot(x, "flags"))
setMethod("getPepAnnot", "pepArrayNorm", function(x) slot(x, "peptideAnnotation"))
setMethod("getSampAnnot", "pepArrayNorm", function(x) slot(x, "sampleAnnotation"))

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





# Plot layout
layout (matrix (1:2, ncol = 1))
# Plot Raw Data - ie. non-control features
palette(rainbow(4))
boxplotData.raw <- x[which(nchar(rownames(x$E)) == 15),]
boxplot(log2(boxplotData.raw$E) ~ boxplotData.raw$genes$Block,
        col = rep(c(1:16), each = 4), 
        pch = 20,
        ylim = c(5, 17),
        main = "Raw Data: Non-Control Sequences",
        xlab = "Print Tip"
        )

# Plot Normalised Data
palette(rainbow(4))
boxplotData.norm <- normdata[which(nchar(rownames(x$E)) == 15),]
boxplot(boxplotData.norm ~ boxplotData.raw$genes$Block,
        col = rep(c(1:16), each = 4), 
        pch = 20,
        ylim = c(5, 17),
        main = "Normalised Data: Non-Control Sequences"
        )


# Plot control seqs
palette(rainbow(4))
boxplotData <- x[rownames(x$E) %in% controlSeq,]
plot(log2(boxplotData$E) ~ boxplotData$genes$Block, 
     col = rep(c(1:16), each = 12), 
     pch = 20,
     main = "Control Sequences"
     )

# Test CV
testSeq <- grep("JPT-control-1", x$genes$Name)
testSeq <- unique(x$genes$ID[testSeq])
testSeq.intensity.raw <- log2(x$E)[x$genes$ID == testSeq]
CV.raw <- sd(testSeq.intensity.raw) / mean(testSeq.intensity.raw)

testSeq.intensity.norm <- normdata[x$genes$ID == testSeq]
CV.norm <- sd(testSeq.intensity.norm) / mean(testSeq.intensity.norm)

CV.raw
CV.norm