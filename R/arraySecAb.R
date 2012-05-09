arraySecAb <- function(x, secabID, transform = "none"){
  if (transform == "none"){
    secab <- assayDataElement(x[ , sampleNames(x) == secabID], "fg")
    arraydata <- assayDataElement(x[ , sampleNames(x) != secabID], "fg")
    
  } else {
    transformExpression <- parse(text = paste(transform, "(y)", sep = ""))
    transformFunc <- function (y){
      eval(transformExpression)
    }
    
    secab <- assayDataElement(x[sampleNames == secabID], "fg")
    secab <- transformFunc(secab)
  }
  fitlm <- apply(log2(RG$R$E), 2, function (x) lm(x ~ log2(RG.sec$R$E)[,1]))
  
}