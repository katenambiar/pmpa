% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/plotSubarrayBlocks-methods.R
\docType{methods}
\name{plotSubarrayBlocks}
\alias{plotSubarrayBlocks}
\alias{plotSubarrayBlocks,MultiSet-method}
\title{Boxplots of array blocks (print tip groups) from peptide microarray data}
\usage{
plotSubarrayBlocks(x, ...)

\S4method{plotSubarrayBlocks}{MultiSet}(x, arr, transform = "log2", ...)
}
\arguments{
\item{x}{MultiSet object with fMedian and/or bMedian matrices
in the assayData slot}

\item{arr}{Index indicating which array should be plotted}

\item{transform}{function to apply to transform the raw data}
}
\value{
plot on current graphics device
}
\description{
Boxplots of array blocks (print tip groups) from peptide microarray data
}

