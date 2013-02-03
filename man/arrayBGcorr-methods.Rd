\docType{methods}
\name{arrayBGcorr}
\alias{arrayBGcorr}
\title{Local background feature correction algorithms}
\arguments{
  \item{x}{MultiSet object with fMedian and bMedian
  matrices in the assayData slot}

  \item{method}{One of 'none', 'subtract', 'ratio' or
  'normexp'}

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

