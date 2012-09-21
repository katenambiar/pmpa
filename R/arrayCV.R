# Calculate the CV of intra-array replicates
# Dependencies: 
# Arguments: x = matrix of intensity values with probes as rows and samples in columns
#            ID = 
# Output:    matrix of CV values with samples in columns and unique probes in rows
#

arrayCV <- function(y, ID){
  sum.y <- rowsum(y, ID, reorder = FALSE, na.rm = TRUE)
  sumsq.y <- rowsum(y^2, ID, reorder = FALSE, na.rm = TRUE)
  n <- rowsum(1L - is.na(y), ID, reorder = FALSE)
  ave <- sum.y/n
  var.y <- ((n * sumsq.y) - sum.y^2)/n^2
  cv <- sqrt(var.y) / ave
  return(cv)
}
