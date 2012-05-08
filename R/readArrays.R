#-----------------------------------------------------------------------------------------------------
#   readArrays
#-----------------------------------------------------------------------------------------------------
#
#' Read peptide microarray raw data files in genepix GPR format
#'
#' This function imports the foreground intensity, background intensity,
#' flags and feature annotation from GPR files and produces an object of class
#' pepArrayPP - an extension of the eSet class.
#'
#' @param files - tab delimited text file with 3 columns - fileName, path and sampleName
#' @param col - a character vector (either "R" to import the 635nm channel or "G" for the 532nm channel)
#' @export
#' @examples
#' ## Not run: files <- dir(pattern="*\\.gpr$")
#' R <- readArrays(files, col = "R")
#' ## End(Not run)
readArrays <- function(files, col = "R") {
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
                             Name = gpr[[1]]$Name,
                             stringsAsFactors = FALSE
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
                             Name = gpr[[1]]$Name,
                             stringsAsFactors = FALSE
                             )
    return(obj)
  }
  
}
