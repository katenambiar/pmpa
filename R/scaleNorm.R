#' Median Scale Normalisation
#' 
#' @param x matrix of intensity values with probes as rows and samples in columns
#' @return matrix of normalised intensities
#' @export
scaleNorm <- function(x){
  y <- apply(x, 2, median, na.rm = TRUE)
  y <- y - exp(mean(log(y)))
  y <- sweep(x, 2, y)
  return(y)
}
