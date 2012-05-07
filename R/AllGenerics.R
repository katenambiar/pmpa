#-----------------------------------------------------------------------------------------------------
#   Generic methods for peparray
#-----------------------------------------------------------------------------------------------------

#' Read annotation files and add them to pepArrayPP or pepArray objects
#'
#' Some additional details go here.
#'
#' @param x Description of \code{x}.
#' @param pheno Description of \code{pheno}.
#' @param protocol Description of \code{protocol}.
#' @param feature Description of \code{feature}.
#'
#' @usage readAnnotation(x, pheno, protocol, feature)
#'
#' @return peparrayPP or peparray object with added annotation
#' 
#' @export
#' @docType methods
#' @rdname readAnnotation-methods
#'
#' @examples ##
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