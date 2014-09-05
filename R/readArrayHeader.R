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
  
  if(!grepl("^ATF", header[[1]][1])){
    stop("Axon Text File (ATF) header not found in file: ", x)
  }
  
  if(any(header[[grep("Wavelengths", header)]] == wavelength)){
    wl.index <- which(header[[grep("Wavelengths", header)]] == wavelength)
  } else {
    stop("Data for ", wavelength, "nm wavelength not found.")
  }
 
  header.df <- data.frame(DateTime = header[[grep("\"DateTime\"", header)]][2],
                          Settings = header[[grep("\"Settings\"", header)]][2],
                          GalFile = header[[grep("\"GalFile\"", header)]][2],
                          ImageFile = header[[grep("\"ImageFiles\"", header)]][wl.index],
                          Wavelength = header[[grep("\"Wavelengths\"", header)]][wl.index],
                          PixelSize = header[[grep("\"PixelSize\"", header)]][2],
                          PMTGain = header[[grep("\"PMTGain\"", header)]][wl.index],
                          ScanPower = header[[grep("\"ScanPower\"", header)]][wl.index],
                          LaserPower = header[[grep("\"LaserPower\"", header)]][wl.index],
                          HeaderLines = i - 1,
                          stringsAsFactors = FALSE
  )

  return(header.df)
}