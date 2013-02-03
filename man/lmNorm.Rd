\name{lmNorm}
\alias{lmNorm}
\title{Linear Model Normalisation}
\usage{
  lmNorm(x, sampleID = colnames(x),
    featureID = rownames(x), controlID,
    model = "Intensity ~ as.factor(ID)")
}
\arguments{
  \item{x}{matrix of intensity values with probes as rows
  and samples in columns}

  \item{sampleID}{vector of sample unique identifiers}

  \item{featureID}{vector of feature identifiers (can
  include repeated features)}

  \item{controlID}{vector of control feature identifiers}

  \item{model}{formula for linear model}
}
\value{
  matrix of normalised intensities
}
\description{
  Linear Model Normalisation
}

