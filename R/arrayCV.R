#' Calculate the CV of intra-array replicates
#' 
#' @param x matrix of intensity values with probes as rows and samples in columns
#' @param ID vector of probe IDs
#' @return matrix of CV values with samples in columns and unique probes in rows
#' @export
arrayCV <- function(x){
  intensity <- fg(x)
  ID <- fData(x)$ID
  flagged.values <- flags(x) == -100
  intensity[flagged.values] <- NA
  
  sum.x <- rowsum(intensity, ID, reorder = FALSE, na.rm = TRUE)
  sumsq.x <- rowsum(intensity^2, ID, reorder = FALSE, na.rm = TRUE)
  n <- rowsum(1L - is.na(intensity), ID, reorder = FALSE)
  ave <- sum.x/n
  var.x <- ((n * sumsq.x) - sum.x^2)/n^2
  cv <- sqrt(var.x) / ave
  return(cv)
}
