#-----------------------------------------------------------------------------------------------------
#   readGPR
#-----------------------------------------------------------------------------------------------------

readGPR <- function(x, path, galPath, col = "R") {
  # Read in peptide microarray GPR files (Genepix) for either 635nm or 532nm scans
  # Dependencies: plyr
  # Arguments:    x = character vector of GPR filenames
  #               path = full path to GPR file directory
  #               galPath = full path to GAL file
  #
  # Author:       Kate Nambiar
  # Last Updated: 1.5.2012
  
  require(plyr)
  if(col == "R"){
    dataHeaders <- c("F635 Median", "B635 Median", "Flags")
    
  } else if(col == "G"){
    dataHeaders <- c("F532 Median", "B532 Median", "Flags")
    
  } else stop ("Colour must be either R or G")
  
  filePath <- as.list(file.path(path, x))
  fileHeaders <- read.table(filePath[[1]], skip = 34, stringsAsFactors = FALSE, nrows = 1)
  idCol <- fileHeaders %in% "ID"
  dataCols <- fileHeaders %in% dataHeaders
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
  
  cat("Reading Array Files Completed")
  
  flags = sapply(GPR, function(x) as.numeric(x$Flags > -99))
  peptideAnnotation = GAL
  sampleAnnotation = data.frame (sampleID = x)
  
  if (col == "R"){
    obj <- new("pepArrayRaw", 
               FG = sapply(GPR, function(x) x$F635.Median),
               BG = sapply(GPR, function(x) x$B635.Median),
               flags = flags,
               peptideAnnotation = peptideAnnotation,
               sampleAnnotation = sampleAnnotation
               )
    return(obj)
  }
  
  if (col == "G"){
    obj <- new("pepArrayRaw", 
               FG = sapply(GPR, function(x) x$F532.Median),
               BG = sapply(GPR, function(x) x$B532.Median),
               flags = flags,
               peptideAnnotation = peptideAnnotation,
               sampleAnnotation = sampleAnnotation
               )
    return(obj)
  }
  
}
