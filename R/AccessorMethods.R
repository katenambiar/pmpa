#' Foreground Intensity Accessor
#' 
#' Extracts the matrix of foreground intensities (fMedian) from a MultiSet object created by \link{readArrays}
#' 
#' @param x MultiSet object
#' @return matrix of foreground intensities
#' @export fg
#' @docType methods 
#' @rdname fg-methods
setGeneric(
  name = "fg", 
  def = function(x) standardGeneric("fg")
)

#' @rdname fg-methods
#' @aliases fg
setMethod(
  f = "fg",
  signature = "MultiSet",
  definition = function(x){
    assayDataElement(x, "fMedian")
  }
  )

#---------------------------------------------------------------------------------------------------------------
#' Background Intensity Accessor
#' 
#' Extracts the matrix of background intensities (bMedian) from a MultiSet object created by \link{readArrays}
#' 
#' @param x MultiSet object
#' @return matrix of background intensities
#' @export bg
#' @docType methods
#' @rdname bg-methods
setGeneric(
  name = "bg", 
  def = function(x) standardGeneric("bg")
  )

#' @rdname bg-methods
#' @aliases bg
setMethod(
  f = "bg",
  signature = "MultiSet",
  definition = function(x){
    assayDataElement(x, "bMedian")
  }
  )


#---------------------------------------------------------------------------------------------------------------
#' Flags Accessor
#' 
#' Extracts the matrix of flagged values from a MultiSet object created by \link{readArrays}
#' 
#' @param x MultiSet object
#' @return matrix of background intensities
#' @export flags
#' @docType methods
#' @rdname flags-methods
setGeneric(
  name = "flags", 
  def = function(x) standardGeneric("flags")
)

#' @rdname flags-methods
#' @aliases flags
setMethod(
  f = "flags",
  signature = "MultiSet",
  definition = function(x){
    assayDataElement(x, "flags")
  }
)