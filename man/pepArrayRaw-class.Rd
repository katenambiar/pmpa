\name{pepArrayRaw-class}
\Rdversion{1.1}
\docType{class}
\alias{pepArrayRaw-class}
\alias{arrayCV,pepArrayRaw-method}
\alias{getBG,pepArrayRaw-method}
\alias{getFG,pepArrayRaw-method}
\alias{getFlags,pepArrayRaw-method}
\alias{getPepAnnot,pepArrayRaw-method}
\alias{getSampAnnot,pepArrayRaw-method}
\alias{show,pepArrayRaw-method}

\title{Class \code{"pepArrayRaw"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pepArrayRaw", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{FG}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{BG}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{flags}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{peptideAnnotation}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{sampleAnnotation}:}{Object of class \code{"data.frame"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{arrayCV}{\code{signature(x = "pepArrayRaw")}: ... }
    \item{getBG}{\code{signature(x = "pepArrayRaw")}: ... }
    \item{getFG}{\code{signature(x = "pepArrayRaw")}: ... }
    \item{getFlags}{\code{signature(x = "pepArrayRaw")}: ... }
    \item{getPepAnnot}{\code{signature(x = "pepArrayRaw")}: ... }
    \item{getSampAnnot}{\code{signature(x = "pepArrayRaw")}: ... }
    \item{show}{\code{signature(object = "pepArrayRaw")}: ... }
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
showClass("pepArrayRaw")
}
\keyword{classes}
