#-----------------------------------------------------------------------------------------------------
#   Generic methods for peparray
#-----------------------------------------------------------------------------------------------------

#' Accesses foreground intensity matrix of pepArrayPP object
#' 
#' @param x pepArrayPP object name
#' 
#' @return matrix of foreground intensities
#' 
#' @export
#' @docType methods
#' @rdname accessor-methods
if(!isGeneric("fg")) {
  setGeneric(
    name = "fg", 
    def = function(x) standardGeneric("fg")
    )
}


#' Accesses background intensity matrix of pepArrayPP object
#' 
#' @param x pepArrayPP object name
#' 
#' @return matrix of background intensities
#' 
#' @export
#' @docType methods
#' @rdname accessor-methods
if(!isGeneric("bg")) {
  setGeneric(
    name = "bg", 
    def = function(x) standardGeneric("bg")
    )
}


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


if(!isGeneric("arrayBGcorr")) {
  setGeneric(
    name = "arrayBGcorr", 
    def = function(x, ...) standardGeneric("arrayBGcorr")
    )
}


if(!isGeneric("intraArrayNorm")) {
  setGeneric(
    name = "intraArrayNorm", 
    def = function(x, ...) standardGeneric("intraArrayNorm")
    )
}

# Calculate the CV of intra-array replicates
if(!isGeneric("arrayCV")) {
  setGeneric(
    name = "arrayCV", 
    def = function(x, ...) standardGeneric("arrayCV")
    )
  }

# Calculate the average of intra-array replicates
if(!isGeneric("arrayAve")) {
  setGeneric(
    name = "arrayAve", 
    def = function(x, ...) standardGeneric("arrayAve")
    )
  }

if(!isGeneric("arraySecAb")) {
  setGeneric(
    name = "arraySecAb", 
    def = function(x, ...) standardGeneric("arraySecAb")
    )
}

# Boxplot
if(!isGeneric("boxplot")) {
  setGeneric("boxplot")
}