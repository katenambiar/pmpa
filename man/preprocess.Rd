\name{preprocess}
\alias{preprocess}
\title{Preprocess peptide microarray data}
\usage{
  preprocess(rawdata, sec.filter = TRUE,
    bg.method = "subtract", norm.method, summary.method,
    ...)
}
\arguments{
  \item{rawdata}{}

  \item{sec.filter}{}
}
\value{
  an object of class ExpressionSet
}
\description{
  \code{preprocess} is a wrapper function that executes the
  4 pre-processing functions (arraySecFilter, arrayBGcorr,
  arayNorm, arraySummary) within the pippa package.
}

