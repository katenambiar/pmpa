#' peparray Accessor Methods
#'
#' Methods for accession and setting of data in pepArrayPP and pepArray slots
#' 
#' @author Kate Nambiar \email{k.z.nambiar@bsms.ac.uk}
#' @aliases peparray
#' @docType methods
#' @keywords methods
#' @rdname accessor-methods
NULL


setMethod(
  f = "exprs",
  signature = "pepArray",
  definition = function(object){
    assayDataElement(object, "exprs")
  }
  )


#' @rdname accessor-methods
#' @aliases fg, pepArrayPP
setMethod(
  f = "fg",
  signature = "pepArrayPP",
  definition = function(x){
    assayDataElement(x, "fg")
  }
  )

#' @rdname accessor-methods
#' @aliases bg, pepArrayPP
setMethod(
  f = "bg",
  signature = "pepArrayPP",
  definition = function(x){
    assayDataElement(x, "bg")
  }
  )