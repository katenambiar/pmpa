readGPR <- function (colour = "RG", targets, gpr.path, gal.filename, gal.path){
  # Read in microarray GPR files (Genepix) for both 635nm and 532nm scans
  # Dependencies: limma
  # Arguments: targets = character vector of GPR filenames
  #            colour = RG, R or G
  #            gpr.path = full path to GPR files in targets
  #            gal.file = GAL file name
  #            gal.path = full path to GAL file
  # Output: (RG) List of Limma EListRaw object with 2 components - R for 635nm and  G for 532nm
  #         (R or G) Limma EListRaw object
  # Last Updated: 18.2.2012
  require(limma)
  if (colour == "RG"){
    RG <- list (R = read.maimages (targets,
                                   path = gpr.path,
                                   source = "genepix",
                                   columns = list(R="F635 Median", Rb="B635 Median"),
                                   wt.fun = function(x) as.numeric(x$Flags > -99)
                                   ),
                G = read.maimages (targets,
                                   path = gpr.path,
                                   source = "genepix",
                                   columns = list(R="F532 Median", Rb="B532 Median"),
                                   wt.fun = function(x) as.numeric(x$Flags > -99)
                                   )
                )
    
    for (i in 1:length(RG)){
      RG[[i]]$genes <- readGAL (galfile = gal.filename, path = gal.path)
      RG[[i]]$printer <- getLayout(readGAL(gal.filename, path = gal.path), guessdups = TRUE)
      rownames(RG[[i]]$E) <- RG[[i]]$genes$ID
    }
    return (RG)
    
  } else if (colour == "R"){
    R <-  read.maimages (targets,
                         path = gpr.path,
                         source = "genepix",
                         columns = list(R="F635 Median", Rb="B635 Median"),
                         wt.fun = function(x) as.numeric(x$Flags > -99)
                         )
    
    R$genes <- readGAL (galfile = gal.filename, path = gal.path)
    R$printer <- getLayout(readGAL(gal.filename, path = gal.path), guessdups = TRUE)
    return (R)
    
  } else if (colour == "G"){
    G <-  read.maimages (targets,
                         path = gpr.path,
                         source = "genepix",
                         columns = list(G="F532 Median", Gb="B532 Median"),
                         wt.fun = function(x) as.numeric(x$Flags > -99)
                         )
    
    G$genes <- readGAL (galfile = gal.filename, path = gal.path)
    G$printer <- getLayout(readGAL(gal.filename, path = gal.path), guessdups = TRUE)
    return (G)
    
  } else stop ("Colour must be either RG, R or G")
}
