#' Reads peptide microarray data from Axon ATF format (Genepix GPR files)
#' 
#' \code{readArrays} is a function used to read in microarray data from Genepix GPR files. 
#' It removes the header information and only imports the median foreground and background
#' intensities as well as other data for QC purposes.
#' 
#' @param files a data with 3 columns: sampleName - unique identifier for the sample, fileName - GPR file name and extension, path - full path or URL to the GPR file
#' @param col either "R" (default) to import data from the 635nm channel or "G" to import 532nm data
#' @return an object of class MultiSet
#' @export
readArrays <- function(files, col = "R") {
  filePath <- file.path(files$path, files$fileName)
  atfHeader <- lapply(filePath, function(x) read.table(x, nrows = 1, stringsAsFactors = FALSE))
  
  if(all(grepl("^ATF", sapply(atfHeader, function(x)x[1,1])))){
    skip <- list()
    for (i in 1:length(filePath)){
      gprHeader <- readLines(filePath[i], n = 100)
      skip[[i]] <- intersect(grep("Name", gprHeader), grep("ID", gprHeader)) - 1
    }
    
  } else {
    missingATF <- files$fileName[which(grepl("^ATF", sapply(atfHeader, function(x)x[1,1])) == FALSE)]
    stop("Axon Text File (ATF) header not found in files: ", missingATF)
    
  }
  
  if (col == "R"){
    dataHeader <- c("F635 Median", 
                    "F635 Mean",
                    "F635 CV",
                    "B635 Median", 
                    "B635 Mean", 
                    "B635 SD", 
                    "F635 % Sat."
                    )
    
  } else if (col == "G"){
    dataHeader <- c("F532 Median", 
                    "F532 Mean",
                    "F532 CV",
                    "B532 Median", 
                    "B532 Mean", 
                    "B532 SD", 
                    "F532 % Sat."
                    )
    
  } else {
    stop("Colour must be 'R' or 'G'")
    
  }
  
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
  
  
  if (col == "R"){
    obj <- new("MultiSet")
    assayData(obj) <- assayDataNew(fMedian = sapply(gpr, function(x) x$F635.Median),
                                   fMean = sapply(gpr, function(x) x$F635.Mean),
                                   fCV = sapply(gpr, function(x) x$F635.CV),
                                   bMedian = sapply(gpr, function(x) x$B635.Median),
                                   bMean = sapply(gpr, function(x) x$B635.Mean),
                                   bSD = sapply(gpr, function(x) x$B635.SD),
                                   fSat = sapply(gpr, function(x) x$F635...Sat.),
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
    return(obj)
  }
  
  if (col == "G"){
    obj <- new("MultiSet") 
    assayData(obj) <- assayDataNew(fMedian = sapply(gpr, function(x) x$F532.Median),
                                   fMean = sapply(gpr, function(x) x$F532.Mean),
                                   fCV = sapply(gpr, function(x) x$F532.CV),
                                   bMedian = sapply(gpr, function(x) x$B532.Median),
                                   bMean = sapply(gpr, function(x) x$B532.Mean),
                                   bSD = sapply(gpr, function(x) x$B532.SD),
                                   fSat = sapply(gpr, function(x) x$F532...Sat.),
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
    return(obj)
  }
  
}
