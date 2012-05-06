setMethod(
  f = "readAnnotation",
  signature = "pepArrayPP",
  definition = function(x, pheno = NULL, protocol = NULL, feature = NULL){
    if (!is.null(pheno)){
      phAnnot <- read.delim(pheno, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), rownames(phAnnot))){
        pData(x) <- combine(pData(x), phAnnot)
      } else {
        stop("Sample names of pepArrayPP object and phenotype annotation do not match")
      }
    }
    
    if (!is.null(protocol)){
      prAnnot <- read.delim(protocol, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), rownames(prAnnot))){
        pData(x) <- combine(pData(x), prAnnot)
      } else {
        stop("Sample names of pepArrayPP object and protocol annotation do not match")
      }
    }
    
    if (!is.null(feature)){
      featureAnnot <- read.delim(feature, stringsAsFactors = FALSE)
    }
    
    return(x)
  }
  )


setMethod(
  f = "readAnnotation",
  signature = "pepArray",
  definition = function(x, ndups, spacing){
    if (!is.null(pheno)){
      phenoAnnot <- read.delim(pheno, stringsAsFactors = TRUE)
    }
    
    if (!is.null(protocol)){
      protocolAnnot <- read.delim(protocol, stringsAsFactors = TRUE)
    }
    
    if (!is.null(feature)){
      featureAnnot <- read.delim(feature, stringsAsFactors = TRUE)
    }
    
  }
  )