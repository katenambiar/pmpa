arrayCV <- function(x, ndups, spacing, weights = NULL){
  # Calculate the CV of intra-array replicates
  # Dependencies: 
  # Arguments: x = matrix of intensity values with probes as rows and samples in columns
  #            ndups = number of technical replicates on the microarray
  #            spacing = number of rows separating a probe from its replicate
  # Output:    matrix of CV values with samples in colums and unique probes in rows
  #
  # Kate Nambiar
  # Last Updated: 30.4.2012
  
  x <- as.matrix(x)
  x[weights == 0] <- NA
  dim(x) <- c(spacing, ndups, ncol(x))
  cv <- apply(x, c(1,3), sd) / apply(x, c(1,3), mean)
  return(cv)
}