#' Preprocess peptide microarray data
#' 
#' \code{preprocess} is a wrapper function that executes the 4 pre-processing functions 
#' (arraySecFilter, arrayBGcorr, arayNorm, arraySummary) within the pippa package.
#' 
#' @param rawdata
#' @param sec.filter
#' @return an object of class ExpressionSet
#' @export
preprocess <- function (rawdata, sec.filter = TRUE, control.arrays = NULL, bg.method = "subtract", bg.offset = 100, norm.method, summary.method){
  if (sec.filter){
    secdata <- arraySecFilter(rawdata, control.arrays, plot = FALSE)
  }
  bgcorrdata <- arrayBGcorr(secdata, method = bg.method, offset = bg.offset)
  normdata <- arrayNorm(bgcorrdata, method = norm.method)
  summarydata <- arraySummary(normdata, method = summary.method)
  return (summarydata)
} 
