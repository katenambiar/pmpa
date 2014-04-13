\name{lmNorm}
\alias{lmNorm}
\title{Linear Model Normalisation}
\usage{
lmNorm(x, sampleID = sampleNames(x), featureID = fData(x)$ID,
  controlID = NULL, include.subarrays = TRUE)
}
\arguments{
  \item{x}{MultiSet Object}

  \item{sampleID}{vector of sample unique identifiers}

  \item{featureID}{vector of feature identifiers (can
  include repeated features)}

  \item{controlID}{vector of control feature identifiers
  (default = NULL)}

  \item{include.subarrays}{boolean - to use subarrays as a
  factor in the linear model (default = TRUE)}
}
\value{
matrix of normalised intensities
}
\description{
This function normalises microarray data by fitting a
linear model and then using the residuals of the fit as the
normalised data. It is normally called from arrayNorm().
}

