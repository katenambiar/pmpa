#-----------------------------------------------------------------------------------------------------
#   Generic methods for peparray
#-----------------------------------------------------------------------------------------------------

# Calculate the CV of intra-array replicates
if(!isGeneric("arrayCV")) {
  setGeneric(
    name = "arrayCV", 
    def = function(x, ...) standardGeneric("arrayCV"),
    useAsDefault = function(x, ndups, spacing){
      x <- as.matrix(x)
      dim(x) <- c(spacing, ndups, ncol(x))
      cv <- apply(x, c(1,3), sd) / apply(x, c(1,3), mean)
      return(cv)
    }
    )
}

