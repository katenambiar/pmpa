#-----------------------------------------------------------------------------------------------------
#   Generic methods for peparray
#-----------------------------------------------------------------------------------------------------

# Protocol data accessor method
if(!isGeneric("prData")) {
  setGeneric(
    name = "prData", 
    def = function(x) standardGeneric("prData")
    )
}


# Read Annotation files for peparray objects
if(!isGeneric("readAnnotation")) {
  setGeneric(
    name = "readAnnotation", 
    def = function(x, ...) standardGeneric("readAnnotation")
    )
}


# Calculate the CV of intra-array replicates
if(!isGeneric("arrayCV")) {
  setGeneric(
    name = "arrayCV", 
    def = function(x, ndups, spacing) standardGeneric("arrayCV"),
    useAsDefault = function(x, ndups, spacing){
      x <- as.matrix(x)
      dim(x) <- c(spacing, ndups, ncol(x))
      cv <- apply(x, c(1,3), sd) / apply(x, c(1,3), mean)
      return(cv)
      }
    )
  }

# Calculate the average of intra-array replicates
if(!isGeneric("arrayAve")) {
  setGeneric(
    name = "arrayAve", 
    def = function(x, ndups, spacing) standardGeneric("arrayAve")
    )
  }