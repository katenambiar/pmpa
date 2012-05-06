#-----------------------------------------------------------------------------------------------------
#   readArrays
#-----------------------------------------------------------------------------------------------------

readArrays <- function(files, col = "R") {
  # Read in peptide microarray GPR files (Genepix) for either 635nm or 532nm scans
  # Dependencies: plyr
  # Arguments:    x = character vector of GPR filenames
  #               path = full path to GPR file directory
  #               galPath = full path to GAL file
  #
  # Author:       Kate Nambiar
  # Last Updated: 1.5.2012
  
  files <- read.delim(files, stringsAsFactors = FALSE)
  filePath <- file.path(files$path, files$fileName)
  atfHeader <- lapply(filePath, function(x) read.table(x, nrows = 2, stringsAsFactors = FALSE))
  
  if (any(sapply(atfHeader, function(x)x[1,1]) == "ATF")){
    skip <- as.numeric(sapply(atfHeader, function(x)x[2,1])) + 2
    ncols <- as.numeric(sapply(atfHeader, function(x)x[2,2]))
    
  } else {
    stop("Invalid Axon Text File (ATF) header")
    
    }
  
  if (col == "R"){
    dataHeader <- c("F635 Median", "B635 Median")
    
  } else if (col == "G"){
    dataHeader <- c("F532 Median", "B532 Median")
    
  } else {
    stop("Colour must be 'R' or 'G'")
    
  }
  
  colHeaders <- list()
  for (i in 1:length(filePath)){
    colHeaders[[i]] <- read.table(filePath[i], skip = skip[i], nrows = 1, stringsAsFactors = FALSE)
    colHeaders[[i]] <- colHeaders[[i]] %in% c("Block", "Column", "Row", "Name", "ID", dataHeader, "Flags")
  }
  
  colClasses <- list()
  for (i in 1:length(filePath)){
    colClasses[[i]] <- rep("NULL", ncols[i])
    colClasses[[i]][colHeaders[[i]]] <- NA
  }
  
  
  gpr <- list()
  for (i in 1:length(filePath)){
    
    cat("Reading GPR file:", filePath[i], "\n")
    gpr[[i]] <- read.table (file = filePath[i], 
                            skip = skip[i], 
                            header = TRUE, 
                            stringsAsFactors = FALSE, 
                            colClasses = colClasses[[i]]
                            )
    }

  cat("Reading", length(filePath), "array files completed")
  
  
  if (col == "R"){
    obj <- new("pepArrayPP")
    assayData(obj) <- assayDataNew(fg = sapply(gpr, function(x) x$F635.Median),
                                   bg = sapply(gpr, function(x) x$B635.Median),
                                   flags = sapply(gpr, function(x) as.numeric(x$Flags > -99))
                                   )
    pData(obj) <- data.frame (fileName = files$fileName,
                             row.names = files$sampleName
                             )
    fData(obj) <- data.frame(ID = gpr[[1]]$ID,
                             Block = gpr[[1]]$Block,
                             Column = gpr[[1]]$Column,
                             Row = gpr[[1]]$Row,
                             Name = gpr[[1]]$Name
                             )
    return(obj)
  }
  
  if (col == "G"){
    obj <- new("pepArrayPP") 
    assayData(obj) <- assayDataNew(fg = sapply(gpr, function(x) x$F532.Median),
                                   bg = sapply(gpr, function(x) x$B532.Median),
                                   flags = sapply(gpr, function(x) as.numeric(x$Flags > -99))
                                   )
    pData(obj) <- data.frame (fileName = files$fileName,
                             row.names = files$sampleName
                             )
    fData(obj) <- data.frame(ID = gpr[[1]]$ID,
                             Block = gpr[[1]]$Block,
                             Column = gpr[[1]]$Column,
                             Row = gpr[[1]]$Row,
                             Name = gpr[[1]]$Name
                             )
    return(obj)
  }
  
}
