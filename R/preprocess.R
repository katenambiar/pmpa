preprocess <- function (rawdata, controlID, bg.method = "subtract", norm.method, summary.method, ...){
  secdata <- arraySecFilter(rawdata, control.arrays = controlID, plot = FALSE)
  bgcorrdata <- arrayBGcorr(secdata, method = bg.method, ...)
  normdata <- arrayNorm(bgcorrdata, method = norm.method)
  summarydata <- arraySummary(normdata, method = summary.method)
  return (summarydata)
} 
