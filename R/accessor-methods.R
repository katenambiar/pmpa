#' Foreground Intensity Accessor
#'
#' Extracts the matrix of foreground intensities (fMedian)
#' from a MultiSet object created by \link{readArrays}
#'
#' @param x MultiSet object
#' @return matrix of foreground intensities
#' @exportMethod fg
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
#-------------------------------------------------------------------------------
#' Background Intensity Accessor
#'
#' Extracts the matrix of background intensities (bg)
#' from a MultiSet object created by \link{readArrays}
#'
#' @param x MultiSet object
#' @return matrix of background intensities
#' @exportMethod bg
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
    assayDataElement(x, "bg")
  }
)
#-------------------------------------------------------------------------------
#' Median Background Intensity Accessor
#'
#' Extracts the matrix of median background intensities (bMedian)
#' from a MultiSet object created by \link{readArrays}
#'
#' @param x MultiSet object
#' @return matrix of median background intensities
#' @exportMethod bmedian
#' @docType methods
#' @rdname bmedian-methods
setGeneric(
  name = "bmedian",
  def = function(x) standardGeneric("bmedian")
)
#' @rdname bmedian-methods
#' @aliases bmedian
setMethod(
  f = "bmedian",
  signature = "MultiSet",
  definition = function(x){
    assayDataElement(x, "bMedian")
  }
)
#-------------------------------------------------------------------------------
#' Mean Background Intensity Accessor
#'
#' Extracts the matrix of mean background intensities (bMean)
#' from a MultiSet object created by \link{readArrays}
#'
#' @param x MultiSet object
#' @return matrix of mean background intensities
#' @exportMethod bmean
#' @docType methods
#' @rdname bmean-methods
setGeneric(
  name = "bmean",
  def = function(x) standardGeneric("bmean")
)
#' @rdname bmean-methods
#' @aliases bmean
setMethod(
  f = "bmean",
  signature = "MultiSet",
  definition = function(x){
    assayDataElement(x, "bMean")
  }
)
#-------------------------------------------------------------------------------
#' Flags Accessor
#'
#' Extracts the matrix of flagged values
#' from a MultiSet object created by \link{readArrays}
#'
#' @param x MultiSet object
#' @return matrix of flagged values
#' @exportMethod flags
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
#-------------------------------------------------------------------------------
#' Diameter Accessor
#'
#' Extracts the matrix of feature diameter values
#' from a MultiSet object created by \link{readArrays}
#'
#' @param x MultiSet object
#' @return matrix of feature diameters
#' @exportMethod dia
#' @docType methods
#' @rdname dia-methods
setGeneric(
  name = "dia",
  def = function(x) standardGeneric("dia")
)
#' @rdname dia-methods
#' @aliases dia
setMethod(
  f = "dia",
  signature = "MultiSet",
  definition = function(x){
    assayDataElement(x, "dia")
  }
)