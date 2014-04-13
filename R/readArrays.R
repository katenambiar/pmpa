#' Read peptide microarray data from GPR files
#' 
#' \code{readArrays} is a function used to read in peptide microarray data from Genepix GPR files (in Axon ATF format). 
#' It produces a Bioconductor MultiSet object with microarray signal intensity data (foreground intensity,
#' background intensity and other measures for assessing feature quality) in the assayData slot. A minimal annotation set
#' is created by recording sample data (sample unique identifier and file name) in the phenoData slot, and feature data 
#' (feature names and ID, and layout information) in the featureData slot. The scan date and time for the GPR file 
#' (if recorded in the GPR header) is written to anannotated data frame in the protocolData slot. The function permits within-array
#' replicate features to have the same ID and name. Only signal intensity data from a single wavelength (single colour data)
#' is imported to the MultiSet object
#' 
#' @param files a data frame with 3 columns: sampleName - unique identifier for the sample, fileName - GPR file name and extension, 
#' path - full path or URL to the directory holding the GPR file
#' @param wavelength integer value for the scan wavelength (typically 635 for Cy5 and 532 for Cy3)
#' @return an object of class MultiSet
#' @export
readArrays <- function(files, wavelength) {
  filePath <- file.path(files$path, files$fileName)
  atfHeader <- lapply(filePath, function(x) read.table(x, nrows = 1, stringsAsFactors = FALSE))
  
  if(all(grepl("^ATF", sapply(atfHeader, function(x)x[1,1])))){
    skip <- list()
    gpr.datetime <- list()
    for (i in 1:length(filePath)){
      gprHeader <- readLines(filePath[i], n = 100)
      skip[[i]] <- intersect(grep("Name", gprHeader), grep("ID", gprHeader)) - 1
      gpr.datetime[[i]] <- gsub("DateTime=", "", grep("DateTime=", scan(filePath[i], what = character(), n = skip[[i]]), value = TRUE))
    }
    
  } else {
    missingATF <- files$fileName[which(grepl("^ATF", sapply(atfHeader, function(x)x[1,1])) == FALSE)]
    stop("Axon Text File (ATF) header not found in files: ", missingATF)
    
  }
  
  dataHeader <- c(paste("F", wavelength, " Median", sep = ""), 
                  paste("F", wavelength, " Mean", sep = ""),
                  paste("F", wavelength, " CV", sep = ""),
                  paste("B", wavelength, " Median", sep = ""), 
                  paste("B", wavelength, " Mean", sep = ""), 
                  paste("B", wavelength, " SD", sep = ""),
                  paste("F", wavelength, " % Sat.", sep = "")
  )
     
  colHeaders <- list()
  for (i in 1:length(filePath)){
    colHeaders[[i]] <- read.table(filePath[i], skip = skip[[i]], nrows = 1, stringsAsFactors = FALSE, sep = "\t")
    colHeaders[[i]] <- colHeaders[[i]] %in% c("Block", "Column", "Row", "Name", "ID", "Dia.", dataHeader, "F Pixels", "Flags")
  }
  
  colClasses <- list()
  ncols <- sapply(colHeaders, length)
  for (i in 1:length(filePath)){
    colClasses[[i]] <- rep("NULL", ncols[i])
    colClasses[[i]][colHeaders[[i]]] <- NA
  }
  
  gpr <- list()
  for (i in 1:length(filePath)){
    
    cat("Reading GPR file:", filePath[i], "\n")
    gpr[[i]] <- read.table (file = filePath[i], 
                            skip = skip[[i]], 
                            header = TRUE, 
                            stringsAsFactors = FALSE, 
                            colClasses = colClasses[[i]],
                            sep = "\t"
    )
  }
  
  cat("Reading", length(filePath), "array files completed")
  
  obj <- new("MultiSet")
  assayData(obj) <- assayDataNew(fMedian = sapply(gpr, function(x) x[,7]),
                                 fMean = sapply(gpr, function(x) x[,8]),
                                 fCV = sapply(gpr, function(x) x[,9]),
                                 bMedian = sapply(gpr, function(x) x[,10]),
                                 bMean = sapply(gpr, function(x) x[,11]),
                                 bSD = sapply(gpr, function(x) x[,12]),
                                 fSat = sapply(gpr, function(x) x[,12]),
                                 fPixels = sapply(gpr, function(x) x$F.Pixels),
                                 dia = sapply(gpr, function(x) x$Dia.),
                                 flags = sapply(gpr, function(x) x$Flags)
  )
  pData(obj) <- data.frame (fileName = files$fileName,
                            row.names = files$sampleName
  )
  fData(obj) <- data.frame(ID = gpr[[1]]$ID,
                           Block = gpr[[1]]$Block,
                           Column = gpr[[1]]$Column,
                           Row = gpr[[1]]$Row,
                           Name = gpr[[1]]$Name,
                           stringsAsFactors = FALSE
                           )
  protocolData(obj) <- AnnotatedDataFrame(data = data.frame(ScanDate = unlist(gpr.datetime)))
  
  return(obj)
   
}