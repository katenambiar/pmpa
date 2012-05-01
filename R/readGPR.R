readGPR <- function(x, path, galPath, col = "RG") {
  # Read in microarray GPR files (Genepix) for both 635nm and 532nm scans
  # Dependencies: plyr
  # Arguments:    x = character vector of GPR filenames
  #               path = full path to GPR file directory
  #               galPath = full path to GAL file
  #
  # Author:       Kate Nambiar
  # Last Updated: 1.5.2012
  
  require(plyr)
  if (col == "RG"){
    dataHeaders <- c("F635 Median", "B635 Median", "F532 Median", "B532 Median", "Flags")
    
  } else if(col == "R"){
    dataHeaders <- c("F635 Median", "B635 Median", "Flags")
    
  } else if(col == "G"){
    dataHeaders <- c("F532 Median", "B532 Median", "Flags")
    
  } else stop ("Colour must be either RG, R or G")
  
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
  
  if (col == "R" | col == "RG"){
    Rfg = sapply(GPR, function(x) x$F635.Median)
    Rbg = sapply(GPR, function(x) x$B635.Median)
  } 
  
  if (col == "G" | col == "RG"){
    Gfg = sapply(GPR, function(x) x$F635.Median)
    Gbg = sapply(GPR, function(x) x$B635.Median)
  }
  
  flags = sapply(GPR, function(x) as.numeric(x$Flags > -99))
  peptideAnnotation = GAL
  sampleAnnotation = data.frame (sampleID = x)

  if (col == "R"){
    R <- new("pepArrayRawR", 
             Rfg = Rfg,
             Rbg = Rbg,
             flags = flags,
             peptideAnnotation = peptideAnnotation,
             sampleAnnotation = sampleAnnotation
             )
    return (R)
    
  } else if (col == "G"){
    R <- new("pepArrayRawG", 
             Gfg = Gfg,
             Gbg = Gbg,
             flags = flags,
             peptideAnnotation = peptideAnnotation,
             sampleAnnotation = sampleAnnotation
             )
    return(R)
    
  } else if (col == "RG"){
    R <- new("pepArrayRawRG", 
             Rfg = Rfg,
             Rbg = Rbg,
             Gfg = Gfg,
             Gbg = Gbg,
             flags = flags,
             peptideAnnotation = peptideAnnotation,
             sampleAnnotation = sampleAnnotation
             )
    return(RG)
  }
  
}
