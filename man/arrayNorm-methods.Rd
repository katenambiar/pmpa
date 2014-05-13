\docType{methods}
\name{arrayNorm}
\alias{arrayNorm}
\alias{arrayNorm,ExpressionSet-method}
\alias{arrayNorm,MultiSet-method}
\title{Normalisation}
\usage{
arrayNorm(x, ...)

\S4method{arrayNorm}{MultiSet}(x, method = "none", ...)

\S4method{arrayNorm}{ExpressionSet}(x, method = "none", ...)
}
\arguments{
  \item{x}{MultiSet object with fMedian and bMedian
  matrices in the assayData slot}

  \item{method}{Character string specifying background
  correction method. Valid methods are 'none', 'subtract',
  'edwards', ratio' or 'normexp'. Defaults to 'none' if no
  method is specified.}

  \item{offset}{numeric value added to raw signal intensity
  before background correction is implemented}

  \item{transform}{Expression to transform raw data.
  Defaults to log2}
}
\value{
MultiSet object with transformed and background corrected
foreground signal in the fMedian matrix
}
\description{
Implements local background correction for peptide
microarray data
}

