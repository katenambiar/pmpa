buildAnnotation <- function(galfile, subarrays = 1) {
  atfHeader <- read.table(galfile, nrows = 2, stringsAsFactors = FALSE)
  
  if(grepl("^ATF", atfHeader[1,1]) == TRUE){
    
    galHeader <- readLines(galfile, n = 100)
    skip <- intersect(grep("Name", galHeader), grep("ID", galHeader)) - 1
    
  } else {
    
    stop("Axon Text File (ATF) header not found")
    
  }
  
  gal <- read.table (file = galfile, 
                     skip = skip, 
                     header = TRUE, 
                     stringsAsFactors = FALSE,
                     sep = "\t"
                     )
  subarrays <- rep(1:subarrays, each = nrow(gal)/subarrays)
  peptideAnnotation <- data.frame(ID = gal$ID,
                                  Subarray = subarrays,
                                  Block = gal$Block,
                                  Row = gal$Row,
                                  Column = gal$Column,
                                  Name = gal$Name,
                                  stringsAsFactors = FALSE
                                  )
  return(peptideAnnotation)
}
  
  