\docType{methods}
\name{arraySecFilter}
\alias{arraySecFilter}
\title{Filter Secondary Antibody Binding Peptide Probes}
\arguments{
  \item{x}{MultiSet object after background correction and
  log transformation}

  \item{control.arrays}{Numeric vector specifying negative
  control (secondary antibody only) arrays.}

  \item{removecontrol.arrays}{either TRUE to remove the
  control array after filtering or FALSE to leave the
  control array in the dataset. (default = TRUE)}

  \item{plot}{Either TRUE to return a histogram with
  overlying density plots of the two mixture components on
  the current graphics device, or FALSE to implement the
  filtration without returning a plot. (default = FALES)}

  \item{...}{further arguments passed to \link{hist}}
}
\value{
  MultiSet object filtered to remove probes with high
  secondary binding
}
\description{
  Implements filtering of signal from peptides that
  specifically bind to the secondary antibody. Using a
  control array incubated just with the secondary antibody,
  significantly binding probes are identified by fitting
  the data to a two-component Gaussian general mixture
  model. A cutoff signal intensity is set at the 95th
  percentile of the distribution from the non-binding
  peptides. Any probes showing signal greater than the
  cutoff are excluded.
}

