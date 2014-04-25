setGeneric(
  name = "annotateArrays", 
  def = function(x, ...) standardGeneric("annotateArrays")
)


setMethod(
  f = "annotateArrays",
  signature = "MultiSet",
  definition = function(x, pheno = NULL, protocol = NULL, feature = NULL){
    if (!is.null(pheno)){
      phAnnot <- read.AnnotatedDataFrame(pheno, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), sampleNames(phAnnot))){
        
        dimLabels(phAnnot) <- c("sampleNames", "sampleColumns")
        phenoData(x) <- BiocGenerics::combine(phenoData(x), phAnnot)
        
      } else {
        stop("Sample names of MultiSet object and phenotype annotation do not match")
      }
    }
    
    if (!is.null(protocol)){
      prAnnot <- read.AnnotatedDataFrame(protocol, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), sampleNames(prAnnot))){
        
        dimLabels(prAnnot) <- c("sampleNames", "sampleColumns")
        protocolData(x) <- BiocGenerics::combine(protocolData(x), prAnnot)
        
      } else {
        stop("Sample names of MultiSet object and protocol annotation do not match")
      }
    }
    
    if (!is.null(feature)){
      if (identical (fData(x)$ID, feature$ID)){
        
        fData(x) <- BiocGenerics::combine(fData(x), feature)
        
      } else {
        stop("Feature IDs of MultiSet object and feature annotation do not match")
      }
    }
    
    return(x)
  }
  )


setMethod(
  f = "annotateArrays",
  signature = "ExpressionSet",
  definition = function(x, pheno = NULL, protocol = NULL, feature = NULL){
    if (!is.null(pheno)){
      phAnnot <- read.AnnotatedDataFrame(pheno, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), rownames(phAnnot))){
        
        dimLabels(phAnnot) <- c("sampleNames", "sampleColumns")
        phenoData(x) <- BiocGenerics::combine(phenoData(x), phAnnot)
        
      } else {
        stop("Sample names of ExpressionSet object and phenotype annotation do not match")
      }
    }
    
    if (!is.null(protocol)){
      prAnnot <- read.AnnotatedDataFrame(protocol, stringsAsFactors = FALSE, row.names = "sampleName")
      if (identical (sampleNames(x), sampleNames(prAnnot))){
        
        dimLabels(prAnnot) <- c("sampleNames", "sampleColumns")
        protocolData(x) <- BiocGenerics::combine(protocolData(x), prAnnot)
        
      } else {
        stop("Sample names of ExpressionSet object and protocol annotation do not match")
      }
    }
    
    if (!is.null(feature)){
      featureAnnot <- read.delim(feature, stringsAsFactors = FALSE)
    }
    
    return(x)
  }
  )