#' MASet Classs
#'
#' Representation of pairwise data as log ratios (M)
#' and log averages (A).
#'
#' @section Slots:
#' \describe{
#'  \item{assayData}{}
#'  \item{phenoData}{An \code{AnnotatedDataFrame}. Annotation for the samples.}
#'  \item{featureData}{}
#'  \item{annotation}{}
#'  \item{protocolData}{Slots inherited from \code{ExpressionSet}.}
#' }
#'
#' @details
#' See \code{?`peptideSet-methods`} for a list of accessors and method associated
#' with the class.
#'
#'
#' @importFrom Biobase
#' @importClassesFrom Biobase eSet
#' @name MASet
#' @rdname MASet
#' @aliases MASet-class
#' @exportClass MASet
#' @author Kate Nambiar
#'
setClass("MASet", 
         contains = "eSet"
         )
