#-----------------------------------------------------------------------------------------------------
#   Generic methods for peparray
#-----------------------------------------------------------------------------------------------------


if(!isGeneric("fg")) {
  #' Accesses foreground intensity matrix of pepArrayPP object
  #' 
  #' @param x A object of class \code{pepArrayPP}
  #' @return matrix of foreground intensities
  #' @author Kate Nambiar \email{k.z.nambiar@bsms.ac.uk}
  #' @keywords methods
  #' @export
  #' @docType methods
  #' @rdname accessor-methods
  setGeneric(
    name = "fg", 
    def = function(x) standardGeneric("fg")
    )
}



if(!isGeneric("bg")) {
  #' Accesses background intensity matrix of pepArrayPP object
  #' 
  #' @param x pepArrayPP object name
  #' 
  #' @return matrix of background intensities
  #' 
  #' @author Kate Nambiar \email{k.z.nambiar@bsms.ac.uk}
  #' @export
  #' @docType methods
  #' @rdname accessor-methods
  setGeneric(
    name = "bg", 
    def = function(x) standardGeneric("bg")
    )
}



if(!isGeneric("readAnnotation")) {
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
  #' @author Kate Nambiar \email{k.z.nambiar@bsms.ac.uk}
  #' @export
  #' @docType methods
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