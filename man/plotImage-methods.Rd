\docType{methods}
\name{plotImage}
\alias{plotImage}
\title{Image plot of microarray data}
\arguments{
  \item{x}{MultiSet object with fMedian and/or bMedian
  matrices in the assayData slot and layout data (block,
  column and row) in the fData slot.}

  \item{lowcol}{COlour associated with low signal
  intensities}

  \item{highcol}{COlour associated with high signal
  intensities}

  \item{ncols}{number of colour shades used}
}
\value{
  MultiSet object with transformed and background corrected
  foreground signal in the fMedian matrix
}
\description{
  Creates image plot representing the microarray foreground
  or background signal intensity organised by the spatial
  location of the spots on the array.
}

