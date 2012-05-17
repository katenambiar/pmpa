\name{pepArrayPP-class}
\Rdversion{1.1}
\docType{class}
\alias{pepArrayPP-class}
\alias{arrayAve,pepArrayPP-method}
\alias{arrayBGcorr,pepArrayPP-method}
\alias{arrayCV,pepArrayPP-method}
\alias{arraySecAb,pepArrayPP-method}
\alias{bg,pepArrayPP-method}
\alias{boxplot,pepArrayPP-method}
\alias{fg,pepArrayPP-method}
\alias{initialize,pepArrayPP-method}
\alias{intraArrayNorm,pepArrayPP-method}
\alias{readAnnotation,pepArrayPP-method}

\title{Class \code{"pepArrayPP"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pepArrayPP", assayData, fg, bg, flags, phenoData, featureData, experimentData, annotation, protocolData, ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{assayData}:}{Object of class \code{"AssayData"} ~~ }
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
    \item{arraySecAb}{\code{signature(x = "pepArrayPP")}: ... }
    \item{bg}{\code{signature(x = "pepArrayPP")}: ... }
    \item{boxplot}{\code{signature(x = "pepArrayPP")}: ... }
    \item{fg}{\code{signature(x = "pepArrayPP")}: ... }
    \item{initialize}{\code{signature(.Object = "pepArrayPP")}: ... }
    \item{intraArrayNorm}{\code{signature(x = "pepArrayPP")}: ... }
    \item{readAnnotation}{\code{signature(x = "pepArrayPP")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("pepArrayPP")
}
\keyword{classes}
