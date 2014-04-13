\name{readArrays}
\alias{readArrays}
\title{Read peptide microarray data from GPR files}
\usage{
readArrays(files, wavelength)
}
\arguments{
  \item{files}{a data frame with 3 columns: sampleName -
  unique identifier for the sample, fileName - GPR file
  name and extension, path - full path or URL to the
  directory holding the GPR file}

  \item{wavelength}{integer value for the scan wavelength
  (typically 635 for Cy5 and 532 for Cy3)}
}
\value{
an object of class MultiSet
}
\description{
\code{readArrays} is a function used to read in peptide
microarray data from Genepix GPR files (in Axon ATF
format). It produces a Bioconductor MultiSet object with
microarray signal intensity data (foreground intensity,
background intensity and other measures for assessing
feature quality) in the assayData slot. A minimal
annotation set is created by recording sample data (sample
unique identifier and file name) in the phenoData slot, and
feature data (feature names and ID, and layout information)
in the featureData slot. The scan date and time for the GPR
file (if recorded in the GPR header) is written to
anannotated data frame in the protocolData slot. The
function permits within-array replicate features to have
the same ID and name. Only signal intensity data from a
single wavelength (single colour data) is imported to the
MultiSet object
}

