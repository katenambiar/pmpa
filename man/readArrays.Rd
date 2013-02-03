\name{readArrays}
\alias{readArrays}
\title{Read peptide microarray data from GPR files}
\usage{
  readArrays(files, col = "R")
}
\arguments{
  \item{files}{a data with 3 columns: sampleName - unique
  identifier for the sample, fileName - GPR file name and
  extension, path - full path or URL to the GPR file}

  \item{col}{either "R" (default) to import data from the
  635nm channel or "G" to import 532nm data}
}
\value{
  an object of class MultiSet
}
\description{
  \code{readArrays} is a function used to read in peptide
  microarray data from Genepix GPR files (in Axon ATF
  format). It produces a Bioconductor MultiSet object with
  microarray signal intensity data in the assayData slot,
  sample data (sample unique identifier and file name) in
  the phenoData slot, and feature data (feature names and
  ID, and layout information) in the featureData slot. The
  function permits within-array replicate features to have
  the same ID and name. Only signal intensity data from a
  single wavelength (single colour data) is imported to the
  MultiSet object
}

