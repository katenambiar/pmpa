plotArrayInfo <- function(x, arr){
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
