\name{pepArrayPP-class}
\Rdversion{1.1}
\docType{class}
\alias{pepArrayPP-class}
\alias{arrayAve,pepArrayPP-method}
\alias{arrayBGcorr,pepArrayPP-method}
\alias{arrayCV,pepArrayPP-method}
\alias{boxplot,pepArrayPP-method}
\alias{initialize,pepArrayPP-method}
\alias{intraArrayNorm,pepArrayPP-method}
\alias{readAnnotation,pepArrayPP-method}

\title{Class \code{"pepArrayPP"}}
\description{
This is a class representation for high-density peptide microarray data undergoing pre-processing. It extends the Biobase \code{"\linkS4class{eSet}"} virtual class.
}
\section{Objects from the Class}{
Objects can be created using the function readArrays to parse data from Genepix GPR files.
}
\section{Slots}{
  \describe{
    \item{\code{assayData}:}{Object of class \code{"AssayData"} Contains 3 matrices with equal dimensions: fg - median foreground signal intensity, bg - median local feature background intensity and flags - indicating values to be discarded in the analysis.}
    \item{\code{phenoData}:}{Object of class \code{"AnnotatedDataFrame"} ~~ }
    \item{\code{featureData}:}{Object of class \code{"AnnotatedDataFrame"} ~~ }
    \item{\code{experimentData}:}{Object of class \code{"MIAxE"} ~~ }
    \item{\code{annotation}:}{Object of class \code{"character"} ~~ }
    \item{\code{protocolData}:}{Object of class \code{"AnnotatedDataFrame"} ~~ }
    \item{\code{.__classVersion__}:}{Object of class \code{"Versions"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{eSet}"}, directly.
Class \code{"\linkS4class{VersionedBiobase}"}, by class "eSet", distance 2.
Class \code{"\linkS4class{Versioned}"}, by class "eSet", distance 3.
}
\section{Methods}{
  \describe{
    \item{arrayAve}{\code{signature(x = "pepArrayPP")}: ... }
    \item{arrayBGcorr}{\code{signature(x = "pepArrayPP")}: ... }
    \item{arrayCV}{\code{signature(x = "pepArrayPP")}: ... }
    \item{boxplot}{\code{signature(x = "pepArrayPP")}: ... }
    \item{initialize}{\code{signature(.Object = "pepArrayPP")}: ... }
    \item{intraArrayNorm}{\code{signature(x = "pepArrayPP")}: ... }
    \item{readAnnotation}{\code{signature(x = "pepArrayPP")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
Kate Nambiar <k.z.nambiar@bsms.ac.uk>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Class \code{"\linkS4class{eSet}"}
Class \code{\linkS4class{pepArray}}
}
\examples{
showClass("pepArrayPP")
}
\keyword{classes}
