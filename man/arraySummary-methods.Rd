\docType{methods}
\name{arraySummary}
\alias{arraySummary}
\title{Summarises intra-array replicates with flagged values excluded}
\arguments{
  \item{x}{MultiSet object}

  \item{method}{c("TukeyBiweight", "MedianPolish", "Mean")}
}
\value{
  ExpressionSet with summarised intensity values in exprs
  slot
}
\description{
  Summarises intra-array replicates with flagged values
  excluded
}

