#-----------------------------------------------------------------------------------------------------
#   Between Array Normalisation
#-----------------------------------------------------------------------------------------------------

setMethod(
  f = "betweenArrayNorm",
  signature = "pepArrayPP",
  definition = function(x, omitCols = NULL, method = "lm"){
    ndata <- fg(x)
    ndata <- as.numeric(ndata)
    ndata <- data.frame(Intensity = ndata, ID = rep(sampleNames(x), each = nrow(x)))
    fit <- lm(Intensity ~ as.factor(ID), data = ndata)
    normdata <- resid(fit) + coef(fit)[1]
    normdata <- matrix(normdata, ncol = ncol(x))
    assayDataElement(x, "fMedian") <- normdata
    return (x)
    }
)