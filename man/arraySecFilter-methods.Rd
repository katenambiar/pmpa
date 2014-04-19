\docType{methods}
\name{arraySecFilter}
\alias{arraySecFilter}
\alias{arraySecFilter,MultiSet-method}
\title{Filter Secondary Antibody Binding Peptide Probes}
\usage{
arraySecFilter(x, ...)

\S4method{arraySecFilter}{MultiSet}(x, control.arrays, bg.offset = 1024,
  control.probes = NULL, cutoff.quantile = 0.95, remove.probes = TRUE,
  remove.control.arrays = TRUE)
}
\arguments{
  \item{x}{MultiSet object after background correction and
  log transformation}

  \item{control.arrays}{Numeric vector specifying negative
  control (secondary antibody only) arrays.}

  \item{cutoff.quantile}{Percentile of the non-binding
  distributiion to set a cutoff (default = 0.95)}

  \item{bg.offset}{A positive constant to offset the
  foreground signal prior to subtractive background
  correction. Improves variance stabilisation for low
  intensity signal. See \link{arrayBGcorr}.}

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
significantly binding probes are identified by fitting the
data to a two-component Gaussian general mixture model. The
signal intensity cutoff is set can be set at any point but
defaults to the 95th percentile of the distribution from
the non-binding peptides. Any probes showing signal greater
than the cutoff are excluded.
}

