# Calculate the CV of intra-array replicates
# Dependencies: 
# Arguments: x = matrix of intensity values with probes as rows and samples in columns
#            ID = 
# Output:    matrix of CV values with samples in columns and unique probes in rows
#

arrayCV <- function(x, ID){
  sum.x <- rowsum(x, ID, reorder = FALSE, na.rm = TRUE)
  sumsq.x <- rowsum(x^2, ID, reorder = FALSE, na.rm = TRUE)
  n <- rowsum(1L - is.na(x), ID, reorder = FALSE)
  ave <- sum.x/n
  var.x <- ((n * sumsq.x) - sum.x^2)/n^2
  cv <- sqrt(var.x) / ave
  return(cv)
}
