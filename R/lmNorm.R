#' Linear Model Normalisation
#' 
#' @param x matrix of intensity values with probes as rows and samples in columns
#' @param sampleID vector of sample unique identifiers
#' @param featureID vector of feature identifiers (can include repeated features)
#' @param controlID vector of control feature identifiers
#' @param model formula for linear model
#' @return matrix of normalised intensities
#' @export
lmNorm <- function(x, sampleID = colnames(x), featureID = rownames(x), controlID, model = "Intensity ~ as.factor(ID)"){
  model <- as.formula(model)
  ctrldata <- x[featureID %in% controlID, ]
  modeldata <- data.frame(Intensity = as.numeric(ctrldata), 
                          ID = rep(sampleID, each = nrow(ctrldata))
  )
  fit <- lm(model, data = modeldata)
  ndata <- resid(fit) + coef(fit)[1]
  ndata <- matrix(ndata, ncol = ncol(ctrldata))
  norm <- ctrldata - ndata
  norm <- matrix(rep(norm[1,], nrow(x)), ncol = ncol(x), byrow = TRUE)
  normdata <- x - norm
  return (normdata)
}
