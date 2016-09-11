#' Array Information from peptide microarray data
#' 
#' @param x MultiSet object with fMedian matrix in the assayData slot
#' @param arr Index indicating which array should be plotted
#' @param subarray Vector of length = 2 indicating 
#' which subarrays should be plotted
#' @param transform function to apply to transform the raw data
#' @return plot on current graphics device
#'  
#' @export
#' @docType methods
#' @rdname plotArrayInfo-methods
setGeneric(
  name = "plotArrayInfo", 
  def = function(x, ...) standardGeneric("plotArrayInfo")
)

#' @rdname plotArrayInfo-methods
#' @aliases plotArrayInfo
setMethod(
  f = "plotArrayInfo",
  signature = "MultiSet",
  definition = function(x, arr){
    arraydata <- x[,arr]
    headerinfo <- pData(protocolData(arraydata))
    
    sampleID <- sampleNames(arraydata)
    fileName <- pData(arraydata)$fileName
    
    plot(0,
         type = "n",
         xlab = "",
         ylab = "",
         axes = FALSE
    )
    text(0.55, 1, paste("Sample Name:", sampleID), pos = 4)
    text(0.55, 0.75, paste("File:", fileName), pos = 4)
    text(0.55, 0.5, paste("Scan Date/Time:", headerinfo$DateTime), pos = 4)
    text(0.55, 0.25, paste("Wavelength: ", headerinfo$Wavelengths, "nm", sep = ""), pos = 4)
    text(0.55, 0, paste("PMT Gain:", headerinfo$PMTGain), pos = 4)
    text(0.55, -0.25, paste("Laser Power: ", headerinfo$ScanPower, "%", sep = ""), pos = 4)
    
  }
)
  