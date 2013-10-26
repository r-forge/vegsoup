\name{VegsoupOptimstride-class}
\Rdversion{1.1}
\docType{class}

\alias{VegsoupOptimstride-class}

\alias{summary,VegsoupOptimstride-methods}

\title{Class \code{"VegsoupOptimstride"}}

\description{
  Class \code{"VegsoupOptimstride"} is a class directly extending class
  \code{\linkS4class{Vegsoup}} and contains a series of calls to \code{\link{VegsoupPartitions}}
  with varing arguments.
}

\section{Objects from the Class}{
  Objects can be created by the class creator function
  \code{\link{OptimStride}}.
}
\section{Slots}{
  \describe{
    \item{\code{optimstride}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Vegsoup}"}, directly.
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "VegsoupOptimstride")}: ... }
	 }
}

\references{
Add references!
}

\author{ Roland Kaiser }


\seealso{
\link{VegsoupPartition}, \linkS4class{VegsoupPartition}
}
\examples{
showClass("VegsoupOptimstride")
}
\keyword{classes}
