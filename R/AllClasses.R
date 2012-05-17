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
#' Pre-processing methods for peptide microarrays [TODO] Complete description
#' 
#' @name peparray-package
#' @aliases peparray
#' @docType package
#' @title Peptide Microarray Pre-Processing and Analysis
#' @author Kate Nambiar \email{k.z.nambiar@bsms.ac.uk}
#' @keywords package
NULL


#' pepArrayPP Class
#'
#' @name pepArrayPP-class
#' @exportClass pepArrayPP
setClass ("pepArrayPP",
          contains = "eSet"
          )


#' pepArray Class
#'
#' @name pepArray-class
#' @exportClass pepArray
setClass ("pepArray",
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

# pepArrayPP Constructor Method
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
