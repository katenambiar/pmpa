readGPR <- function(x, path, galPath) {
  # Read in microarray GPR files (Genepix) for both 635nm and 532nm scans
  # Dependencies: plyr
  # Arguments:    x = character vector of GPR filenames
  #               path = full path to GPR file directory
  #               galPath = full path to GAL file
  #
  # Author:       Kate Nambiar
  # Last Updated: 29.4.2012
  
  require(plyr)
  
  filePath <- as.list(file.path(path, x))
  fileHeaders <- read.table(filePath[[1]], skip = 34, stringsAsFactors = FALSE, nrows = 1)
  idCol <- fileHeaders %in% "ID"
  dataCols <- fileHeaders %in% c("F635 Median", "B635 Median", "F532 Median", "B532 Median", "Flags")
  cols <- rep("NULL", length(fileHeaders))
  cols[idCol] <- "character"
  cols[dataCols] <- "integer"
  
  cat("Reading GPR files... \n")
  GPR <- llply(filePath, function(x) read.table(x, header = TRUE, skip = 34, stringsAsFactors = FALSE, colClasses = cols), .progress = "text")
  names(GPR) <- x
  
  cat("Reading GAL file... \n \n")
  GAL <- readLines(galPath)
  skip <- intersect(grep("Name", GAL), grep("ID", GAL)) - 1
  GAL <- read.table(galPath, header = TRUE, skip = skip, stringsAsFactors = FALSE)
  
  listOut <- list(F635 = sapply(GPR, function(x) x$F635.Median),
                  B635 = sapply(GPR, function(x) x$B635.Median),
                  F532 = sapply(GPR, function(x) x$F532.Median),
                  B532 = sapply(GPR, function(x) x$B532.Median),
                  Weights = sapply(GPR, function(x) as.numeric(x$Flags > -99)),
                  peptideAnnotation = GAL
                  )
  cat("Reading Array Files Completed")
  return(listOut)
}
