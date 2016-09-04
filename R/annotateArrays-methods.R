#' Adds annotation to peptide microarray data
#' 
#' 
#' 
#' @param x MultiSet object
#' @return Multiset object
#' @exportMethod annotateArrays
#' @docType methods 
#' @rdname annotateArrays-methods
setGeneric(
  name = "annotateArrays", 
  def = function(x, ...) standardGeneric("annotateArrays")
)


#' @rdname annotateArrays-methods
#' @aliases annotateArrays
setMethod(
  f = "annotateArrays",
  signature = "MultiSet",
  definition = function(x, pheno = NULL, protocol = NULL, feature = NULL){
    if (!is.null(pheno)){
      phAnnot <- as(pheno, "AnnotatedDataFrame")
      if (identical (sampleNames(x), sampleNames(phAnnot))){
        
        dimLabels(phAnnot) <- c("sampleNames", "sampleColumns")
        phenoData(x) <- BiocGenerics::combine(phenoData(x), phAnnot)
        
      } else {
        stop("Sample names of MultiSet object and phenotype annotation do not match")
      }
    }
    
    if (!is.null(protocol)){
      prAnnot <- as(protocol, "AnnotatedDataFrame")
      if (identical (sampleNames(x), sampleNames(prAnnot))){

        protocolData(x) <- BiocGenerics::combine(protocolData(x), prAnnot)
        
      } else {
        stop("Sample names of MultiSet object and protocol annotation do not match")
      }
    }
    
    if (!is.null(feature)){
      if (identical (featureNames(x), 
                     sprintf("%s_%i_%i_%i", 
                             feature$ID, 
                             feature$Block, 
                             feature$Column, 
                             feature$Row))){
        
        rownames(feature) <- 
          sprintf("%s_%i_%i_%i", 
                  feature$ID, 
                  feature$Block, 
                  feature$Column, 
                  feature$Row
                  )
        fData(x) <- BiocGenerics::combine(fData(x), feature)
        
      } else {
        stop("Feature IDs of MultiSet object and feature annotation do not match")
      }
    }
    
    return(x)
  }
  )


#' @rdname annotateArrays-methods
#' @aliases annotateArrays
setMethod(
  f = "annotateArrays",
  signature = "ExpressionSet",
  definition = function(x, pheno = NULL, protocol = NULL, feature = NULL){
    if (!is.null(pheno)){
      phAnnot <- read.AnnotatedDataFrame(pheno, 
                                         stringsAsFactors = FALSE, 
                                         row.names = "sampleName"
                                         )
      if (identical (sampleNames(x), rownames(phAnnot))){
        
        dimLabels(phAnnot) <- c("sampleNames", "sampleColumns")
        phenoData(x) <- BiocGenerics::combine(phenoData(x), phAnnot)
        
      } else {
        stop("Sample names of ExpressionSet object and phenotype annotation do not match")
      }
    }
    
    if (!is.null(protocol)){
      prAnnot <- read.AnnotatedDataFrame(protocol, 
                                         stringsAsFactors = FALSE, 
                                         row.names = "sampleName"
                                         )
      if (identical (sampleNames(x), sampleNames(prAnnot))){
        
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
