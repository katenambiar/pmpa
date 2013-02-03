\name{arrayCV}
\alias{arrayCV}
\title{Calculate the CV of intra-array replicates}
\usage{
  arrayCV(x, ID)
}
\arguments{
  \item{x}{matrix of intensity values with probes as rows
  and samples in columns}

  \item{ID}{vector of probe IDs}
}
\value{
  matrix of CV values with samples in columns and unique
  probes in rows
}
\description{
  Calculate the CV of intra-array replicates
}

