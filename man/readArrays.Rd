\name{readArrays}
\alias{readArrays}
\title{Read peptide microarray raw data files in genepix GPR format}
\usage{
  readArrays(files, col = "R")
}
\arguments{
  \item{files}{tab delimited text file with 3 columns -
  fileName, path and sampleName}

  \item{col}{a character vector (either "R" to import the
  635nm channel or "G" for the 532nm channel)}
}
\description{
  This function imports the foreground intensity,
  background intensity, flags and feature annotation from
  GPR files and produces an object of class pepArrayPP - an
  extension of the eSet class.
}

