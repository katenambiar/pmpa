#-----------------------------------------------------------------------------------------------------
#   readArrays
#-----------------------------------------------------------------------------------------------------

readArrays <- function(protocol, sample, experiment = NULL, col = "R") {
  # Read in peptide microarray GPR files (Genepix) for either 635nm or 532nm scans
  # Dependencies: plyr
  # Arguments:    x = character vector of GPR filenames
  #               path = full path to GPR file directory
  #               galPath = full path to GAL file
  #
  # Author:       Kate Nambiar
  # Last Updated: 1.5.2012
  
  require(plyr)
  samples <- read.delim(samples,
                        stringsAsFactors = FALSE
                        )
  
  filePath <- as.list(file.path(protocol$path, protocol$fileName))
  
  
  if(col == "R"){
    dataHeaders <- c("F635 Median", "B635 Median", "Flags")
    
  } else if(col == "G"){
    dataHeaders <- c("F532 Median", "B532 Median", "Flags")
    
  } else stop ("Colour must be either R or G")
  

  fileHeaders <- read.table(filePath[[1]], skip = 34, stringsAsFactors = FALSE, nrows = 1)
  idCol <- fileHeaders %in% "ID"
  dataCols <- fileHeaders %in% dataHeaders
  cols <- rep("NULL", length(fileHeaders))
  cols[idCol] <- "character"
  cols[dataCols] <- "integer"
  
  cat("Reading GPR files... \n")
  GPR <- llply(filePath, function(x) read.table(x, header = TRUE, skip = 34, stringsAsFactors = FALSE, colClasses = cols), .progress = "text")
  cat("Reading Array Files Completed")
  
  
  if (col == "R"){
    obj <- new("pepArrayPP")
    assayData(obj) <- assayDataNew(fg = sapply(GPR, function(x) x$F635.Median),
                                   bg = sapply(GPR, function(x) x$B635.Median),
                                   flags = sapply(GPR, function(x) as.numeric(x$Flags > -99))
                                   )
    pData(obj) <- data.frame(samples[,-2],
                             row.names = samples$sampleName
                             )
    

    return(obj)
  }
  
  if (col == "G"){
    obj <- new("pepArrayPP") 
    assayData(obj) <- assayDataNew(fg = sapply(GPR, function(x) x$F532.Median),
                                   bg = sapply(GPR, function(x) x$B532.Median),
                                   flags = sapply(GPR, function(x) as.numeric(x$Flags > -99))
                                   )
    
    return(obj)
  }
  
}
