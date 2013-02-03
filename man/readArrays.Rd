\name{readArrays}
\alias{readArrays}
\title{Reads peptide microarray data from Axon ATF format (Genepix GPR files)}
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
  \code{readArrays} is a function used to read in
  microarray data from Genepix GPR files. It removes the
  header information and only imports the median foreground
  and background intensities as well as other data for QC
  purposes.
}

