#' Read peptide microarray data from GPR files
#' 
#' \code{readArrayHeader} is usually called by \code{readArrays} rather than being called directly by the user.
#' 
#' @param x filename including full path for the array header to be read
#' @param wavelength integer value for the scan wavelength (typically 635 for Cy5 and 532 for Cy3)
#' @return data frame
#' @export
readArrayHeader <- function(x, wavelength){
  con <- file(x, open = "r")
  on.exit(close(con))

  i <- 0
  header <- list()
  repeat{
    i <- i + 1
    header.lines <- readLines(con, n = 1)
    header[[i]] <- strsplit(sub("^\"", "", sub("\"$", "", header.lines)), split = "=|\t")[[1]]
    if(grepl("Name", header.lines) & grepl("ID", header.lines)){
      break
    }
  }
  header[[i]] <- NULL
  
  if(any(header[[grep("Wavelengths", header)]] == wavelength)){
    wl.index <- which(header[[grep("Wavelengths", header)]] == wavelength)
  } else {
    stop("Data for ", wavelength, "nm wavelength not found.")
  }
  
  header.df <- data.frame(Reduce(rbind, header), 
                          HeaderLines = i - 1,
                          row.names = 1)
    
  return(header.df)
}