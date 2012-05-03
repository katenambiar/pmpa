#-----------------------------------------------------------------------------------------------------
#   Definitions for pepArrayRaw class
#-----------------------------------------------------------------------------------------------------

# pepArrayRaw Class Definition
setClass ("pepArrayRaw", 
          representation (FG = "matrix", BG = "matrix", flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )

#-----------------------------------------------------------------------------------------------------
#   Accessor Methods for pepArrayRaw class
#-----------------------------------------------------------------------------------------------------

setMethod("getFG", "pepArrayRaw", function(x) slot(x, "FG"))
setMethod("getBG", "pepArrayRaw", function(x) slot(x, "BG"))
setMethod("getFlags", "pepArrayRaw", function(x) slot(x, "flags"))
setMethod("getPepAnnot", "pepArrayRaw", function(x) slot(x, "peptideAnnotation"))
setMethod("getSampAnnot", "pepArrayRaw", function(x) slot(x, "sampleAnnotation"))

#-----------------------------------------------------------------------------------------------------
#   Print Methods for pepArrayRaw class
#-----------------------------------------------------------------------------------------------------

setMethod("show", "pepArrayRaw", 
          function(object) {
            cat("Object of class ", class(object), "\n", sep = "")
            cat(ncol(getFG(object)), " Samples \n", sep = "")
            cat(nrow(getFG(object)), " Probes \n", sep = "")
          })

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

#-----------------------------------------------------------------------------------------------------
#   ArrayCV
#-----------------------------------------------------------------------------------------------------


setMethod(
  f = "arrayCV",
  signature = "pepArrayRaw",
  definition = function(x, ndups, spacing){
    # Calculate the CV of intra-array replicates
    # Dependencies: 
    # Arguments: x = PepArrayRaw object or matrix of intensity values with probes as rows and samples in columns
    #            ndups = number of technical replicates on the microarray
    #            spacing = number of rows separating a probe from its replicate
    # Output:    matrix of CV values with samples in columns and unique probes in rows
    #
    # Kate Nambiar
    # Last Updated: 30.4.2012
    y <- log2(getFG(x))
    y[flags(x) == 0] <- NA
    dim(y) <- c(spacing, ndups, ncol(y))
    cv <- apply(y, c(1,3), sd) / apply(y, c(1,3), mean)
    return(cv)
    }
  )

  setMethod(
    f = "arrayCV",
    signature = "matrix",
    definition = function(x, ndups, spacing){
      # Calculate the CV of intra-array replicates
      # Dependencies: 
      # Arguments: x = PepArrayRaw object or matrix of intensity values with probes as rows and samples in columns
      #            ndups = number of technical replicates on the microarray
      #            spacing = number of rows separating a probe from its replicate
      # Output:    matrix of CV values with samples in columns and unique probes in rows
      #
      # Kate Nambiar
      # Last Updated: 30.4.2012
      dim(x) <- c(spacing, ndups, ncol(x))
      cv <- apply(x, c(1,3), sd) / apply(x, c(1,3), mean)
      return(cv)
    })

