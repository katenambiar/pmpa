setMethod(
  f = "readAnnotation",
  signature = "MultiSet",
  definition = function(x, pheno = NULL, protocol = NULL, feature = NULL){
    if (!is.null(pheno)){
      phAnnot <- read.AnnotatedDataFrame(pheno, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), sampleNames(phAnnot))){
        
        dimLabels(phAnnot) <- c("sampleNames", "sampleColumns")
        phenoData(x) <- combine(protocolData(x), phAnnot)
        
      } else {
        stop("Sample names of pepArrayPP object and phenotype annotation do not match")
      }
    }
    
    if (!is.null(protocol)){
      prAnnot <- read.AnnotatedDataFrame(protocol, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), sampleNames(prAnnot))){
        
        dimLabels(prAnnot) <- c("sampleNames", "sampleColumns")
        protocolData(x) <- combine(protocolData(x), prAnnot)
        
      } else {
        stop("Sample names of pepArrayPP object and protocol annotation do not match")
      }
    }
    
    if (!is.null(feature)){
      featureAnnot <- read.delim(feature, stringsAsFactors = FALSE)
      if (identical (fData(x)$ID, featureAnnot$ID)){
        
        fData(x) <- combine(fData(x), featureAnnot)
        
      } else {
        stop("Feature IDs of pepArrayPP object and feature annotation do not match")
      }
    }
    
    return(x)
  }
  )


setMethod(
  f = "readAnnotation",
  signature = "ExpressionSet",
  definition = function(x, pheno = NULL, protocol = NULL, feature = NULL){
    if (!is.null(pheno)){
      phAnnot <- read.AnnotatedDataFrame(pheno, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), rownames(phAnnot))){
        
        dimLabels(phAnnot) <- c("sampleNames", "sampleColumns")
        phenoData(x) <- combine(protocolData(x), phAnnot)
        
      } else {
        stop("Sample names of pepArrayPP object and phenotype annotation do not match")
      }
    }
    
    if (!is.null(protocol)){
      prAnnot <- read.AnnotatedDataFrame(protocol, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), sampleNames(prAnnot))){
        
        dimLabels(prAnnot) <- c("sampleNames", "sampleColumns")
        protocolData(x) <- combine(protocolData(x), prAnnot)
        
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