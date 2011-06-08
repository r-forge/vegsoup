\name{VegsoupDataPartitionFidelity-class}
\Rdversion{1.1}
\docType{class}
\alias{VegsoupDataPartitionFidelity-class}
\alias{getStat,VegsoupDataPartitionFidelity-method}
\alias{Latex,VegsoupDataPartitionFidelity-method}
\alias{summary,VegsoupDataPartitionFidelity-method}

\title{Class "VegsoupDataPartitionFidelity"}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("VegsoupDataPartitionFidelity", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{stat}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{fisher.test}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{lowerCI}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{upperCI}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{nboot}:}{Object of class \code{"integer"} ~~ }
    \item{\code{method}:}{Object of class \code{"character"} ~~ }
    \item{\code{part}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{k}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{dist}:}{Object of class \code{"character"} ~~ }
    \item{\code{binary}:}{Object of class \code{"logical"} ~~ }
    \item{\code{species}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{sites}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{species.long}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{sites.long}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{taxonomy}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{scale}:}{Object of class \code{"list"} ~~ }
    \item{\code{layers}:}{Object of class \code{"character"} ~~ }
    \item{\code{group}:}{Object of class \code{"integer"} ~~ }
    \item{\code{sp.points}:}{Object of class \code{"SpatialPointsDataFrame"} ~~ }
    \item{\code{sp.polygons}:}{Object of class \code{"SpatialPolygonsDataFrame"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{VegsoupDataPartition}"}, directly.
Class \code{"\linkS4class{VegsoupData}"}, by class "VegsoupDataPartition", distance 2.
Class \code{"\linkS4class{Vegsoup}"}, by class "VegsoupDataPartition", distance 3.
}
\section{Methods}{
  \describe{
    \item{getStat}{\code{signature(obj = "VegsoupDataPartitionFidelity")}: ... }
    \item{Latex}{\code{signature(object = "VegsoupDataPartitionFidelity")}: ... }
    \item{summary}{\code{signature(object = "VegsoupDataPartitionFidelity")}: ... }
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
showClass("VegsoupDataPartitionFidelity")
}
\keyword{classes}
