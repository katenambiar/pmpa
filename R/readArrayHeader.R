#' Read peptide microarray data from GPR files
#' 
#' \code{readArrayHeader} is usually called by \code{readArrays} rather than being called directly by the user.
#' 
#' @param x filename including full path for the array header to be read
#' @return data frame
#' @export
readArrayHeader <- function(x){
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
  
  header.df <- data.frame(Reduce(rbind, header), row.names = 1)

  return(header.df)
}