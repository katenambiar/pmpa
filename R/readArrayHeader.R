#' Read peptide microarray data from GPR files
#' 
#' \code{readArrayHeader} is usually called by \code{readArrays} rather than being called directly by the user.
#' 
#' @param x filename including full path for the array header to be read
#' @return data frame
#' @export
readArrayHeader <- function(x, wavelength, wavelength.field = "Wavelengths"){
  con <- file(x, open = "r")
  on.exit(close(con))

  # Read header lines from GPR file
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
  
  # Check ATF header length - warning issued if mismatch (potential corrupted or edited header)
  if (as.integer(header[[2]][1]) + 2 != length(header)){
    warning("ATF header length field in", basename(x), "incorrect. Header length of", length(header), "lines used.")
  }
  
  header[[2]] <- c("HeaderLines", length(header))
  
  
  wl.row <- which(sapply(header, function(y) y[1]) == wavelength.field)
  
  if (!length(wl.row)){
    warning("Wavelength field not found - default header values used.")
    header.data <- sapply(header, function(y) y[2])
    header.df <- data.frame(header.data, row.names = sapply(header, function(y) y[1]))
    return (header.df)
    
  } else {
    
    wl.col <- which(header[[wl.row]] == wavelength)
    wl.data <- sapply(header, function(y) y[wl.col])
    wl.data <- wl.data[!is.na(wl.data)]
    
    header.data <- sapply(header, function(y) y[2])
    header.data[!is.na(sapply(header, function(y) y[wl.col]))] <- wl.data
    header.df <- data.frame(header.data, row.names = sapply(header, function(y) y[1]))
    return(header.df)    
  }
}