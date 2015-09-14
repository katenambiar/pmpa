#' Scale Normalisation using GMM
#' 
#' @param x matrix of intensity values with 
#' probes as rows and samples in columns
#' @return matrix of normalised intensities
#' @export
scaleNormGMM <- function(x){
  
  model.gmm <- list()
  for (i in 1:ncol(x)){
    set.seed(123)
    model.gmm[[i]] <- normalmixEM(x[,i], k = 2, maxit = 100, 
                                  epsilon = 0.001, fast = TRUE)
  }
  
  mean.model.gmm <- sapply(model.gmm, function(x) x$mu[1])
  y <- mean.model.gmm - exp(mean(log(abs(mean.model.gmm))))
  z <- sweep(x, 2, y) 
  return(z)
}
