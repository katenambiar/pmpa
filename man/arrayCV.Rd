\name{arrayCV}
\alias{arrayCV}
\title{Calculate the CV of intra-array replicates}
\usage{
arrayCV(x)
}
\arguments{
  \item{x}{MultiSet object with probe identifiers stored in
  fData(x)$ID}
}
\value{
matrix of CV values with samples in columns and unique
probes in rows
}
\description{
Calculate the CV of intra-array replicates
}

