setGeneric(
  name = "meanEpitopeWindow", 
  def = function(arraydata, ...) standardGeneric("meanEpitopeWindow")
)

setMethod(
  f = "meanEpitopeWindow",
  signature = "ExpressionSet",
  definition = function(arraydata, epitope.length, protein.sequence) {
    
    peptide <- featureNames(arraydata)
    
    window.seqs <- sapply(seq(1,nchar(protein.sequence),1), function(i) substr(protein.sequence, i, i + epitope.length - 1))
    window.seqs <- window.seqs[-which(nchar(window.seqs) < epitope.length)]
    
    index <- sapply(window.seqs, function(x) grep(x, peptide), simplify = FALSE)
    window.intensity <- lapply(index, function(x) exprs(arraydata)[x, ])
    
    window.mean.intensity <- lapply(window.intensity, function(x){
      if(is.matrix(x)){
        y <-  colMeans(x)
        return(y)
      } else{
        return(x)
      }
    }
    )
    
    window.mean.intensity <- do.call(rbind.data.frame, window.mean.intensity)
    names(window.mean.intensity) <- sampleNames(arraydata)
    
    return(window.mean.intensity)
  }
)

setMethod(
  f = "meanEpitopeWindow",
  signature = "matrix",
  definition = function(arraydata, epitope.length, protein.sequence) {
    
    peptide <- rownames(arraydata)
    
    window.seqs <- sapply(seq(1,nchar(protein.sequence),1), function(i) substr(protein.sequence, i, i + epitope.length - 1))
    window.seqs <- window.seqs[-which(nchar(window.seqs) < epitope.length)]
    
    index <- sapply(window.seqs, function(x) grep(x, peptide), simplify = FALSE)
    window.intensity <- lapply(index, function(x) arraydata[x, ])
    
    window.mean.intensity <- lapply(window.intensity, function(x){
      if(is.matrix(x)){
        y <-  colMeans(x)
        return(y)
      } else{
        return(x)
      }
    }
    )
    
    window.mean.intensity <- do.call(rbind.data.frame, window.mean.intensity)
    colnames(window.mean.intensity) <- colnames(arraydata)
    
    return(window.mean.intensity)
  }
)

