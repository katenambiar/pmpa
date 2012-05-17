#' Peptide Microarray Pre-Processing and Analysis
#'
#' \tabular{ll}{
#' Package: \tab peparray\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2012-05-17\cr
#' Licence: \tab GPL-2\cr
#' }
#'
#' [TODO] Complete description
#' 
#' @name peparray-package
#' @aliases peparray peparray-package
#' @docType package
#' @title Peptide Microarray Pre-Processing and Analysis
#' @author Kate Nambiar \email{k.z.nambiar@@bsms.ac.uk}
#' @keywords package
NULL


#' pepArrayPP Class
#'
#' Class representation for peptide microarray data and annotation undergoing pre-processing.
#' Extends the \code{eSet} class.
#' assayData Object of class \code{assayData} containing raw and/or pre-processed data. 
#' A list of 3 matrices: fg - Foreground raw intensity, bg - background raw intensity, flags - spot weighting flags 
#' phenoData Object of class \code{AnnotatedDataFrame} containing phenotype annotation
#'
#' @name pepArrayPP-class
#' @exportClass pepArrayPP
setClass ("pepArrayPP",
          contains = "eSet"
          )

# pepArrayPP Constructor Method
setMethod ("initialize", "pepArrayPP",
           function(.Object,
                    assayData = assayDataNew(fg = fg, bg = bg, flags = flags),
                    fg = new("matrix"),
                    bg = new("matrix"),
                    flags = new("matrix"),
                    phenoData = new("AnnotatedDataFrame"),
                    featureData = new("AnnotatedDataFrame"),
                    experimentData = new("MIAME"),
                    annotation = character(),
                    protocolData = new("AnnotatedDataFrame"),
                    ...
                    ) {
             
             if(!missing(assayData) && any(!missing(fg), !missing(bg), !missing(flags))){
               warning("Using 'assayData'; ignoring 'fg', 'bg', 'flags'")
             }
             callNextMethod(.Object, 
                            assayData = assayData,
                            phenoData = phenoData,
                            featureData = featureData,
                            experimentData = experimentData,
                            annotation = annotation,
                            protocolData = protocolData,
                            ...)
           })


#--------------------------------------------------------------------------------------------


#' pepArray Class
#'
#' Class representation for peptide microarray data and annotation undergoing pre-processing.
#' Extends the \code{eSet} class.
#' assayData Object of class \code{assayData} containing pre-processed data. 
#' phenoData Object of class \code{AnnotatedDataFrame} containing phenotype annotation
#'
#' @name pepArray-class
#' @exportClass pepArray
setClass ("pepArray",
          contains = "eSet"
          )


# pepArray Constructor Method
setMethod ("initialize", "pepArray",
           function(.Object,
                    assayData = assayDataNew(exprs = exprs),
                    exprs = new("matrix"),
                    exprs.se = new("matrix"),
                    ...
                    ) {
             
             if(!missing(assayData) && any(!missing(exprs), !missing(exprs.se))){
               warning("Using 'assayData'; ignoring 'exprs', 'exprs.se'")
             }
             callNextMethod(.Object, 
                            assayData = assayData,
                            ...)
           })
