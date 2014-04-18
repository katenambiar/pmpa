#' Preprocess peptide microarray data
#' 
#' \code{preprocess} is a wrapper function that executes the 4 pre-processing functions 
#' (arraySecFilter, arrayBGcorr, arayNorm, arraySummary) within the pippa package.
#' 
#' @param rawdata
#' @param sec.filter
#' @return an object of class ExpressionSet
#' @export
preprocess <- function (rawdata, sec.filter = TRUE, control.arrays = NULL, remove.probes = FALSE, bg.method = "subtract", bg.offset = 100, norm.method, summary.method){
  if (sec.filter){
    secdata <- arraySecFilter(rawdata, control.arrays, remove.probes = remove.probes, plot = FALSE)
  }
  bgcorrdata <- arrayBGcorr(secdata, method = bg.method, offset = bg.offset)
  normdata <- arrayNorm(bgcorrdata, method = norm.method)
  cv <- arrayCV(normdata)
  cv20 <- unique(names(unlist(apply(cv, 2, function(x) x[which(x > 0.20)]))))
  summarydata <- arraySummary(normdata, method = summary.method)
  summarydata <- summarydata[!featureNames(summarydata) %in% cv20, ]
  return (summarydata)
} 
