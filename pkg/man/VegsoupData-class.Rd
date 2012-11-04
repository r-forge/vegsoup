\name{VegsoupData-class}
\Rdversion{1.1}
\docType{class}
\alias{VegsoupData-class}
\alias{[,VegsoupData,ANY,ANY,missing-method}
\alias{$,VegsoupData-method}
\alias{Abbreviation,VegsoupData-method}
\alias{Arrange,VegsoupData-method}
\alias{as.binary,VegsoupData-method}
\alias{as.character,VegsoupData-method}
\alias{as.numeric,VegsoupData-method}
\alias{coerce,VegsoupData,list-method}
\alias{colSums,VegsoupData-method}
\alias{DecomposeNames,VegsoupData-method}
\alias{dim,VegsoupData-method}
\alias{getDistconnected,VegsoupData-method}
\alias{head,VegsoupData-method}
\alias{Indpower,VegsoupData-method}
\alias{Indspc,VegsoupData-method}
\alias{KML,VegsoupData-method}
\alias{Layers,VegsoupData-method}
\alias{names,VegsoupData-method}
\alias{ncol,VegsoupData-method}
\alias{nrow,VegsoupData-method}
\alias{plot,VegsoupData,missing-method}
\alias{Richness,VegsoupData-method}
\alias{rownames,VegsoupData,missing,missing-method}
\alias{rowSums,VegsoupData-method}
\alias{SampleVegsoup,VegsoupData-method}
\alias{Sites,VegsoupData-method}
\alias{Sites<-,VegsoupData,data.frame-method}
\alias{Stride,VegsoupData-method}
\alias{summary,VegsoupData-method}
\alias{tail,VegsoupData-method}

\title{Class \code{"VegsoupData"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("VegsoupData", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
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
Class \code{"\linkS4class{Vegsoup}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "VegsoupData", i = "ANY", j = "ANY", drop = "missing")}: ... }
    \item{$}{\code{signature(x = "VegsoupData")}: ... }
    \item{Abbreviation}{\code{signature(obj = "VegsoupData")}: ... }
    \item{Arrange}{\code{signature(object = "VegsoupData")}: ... }
    \item{as.binary}{\code{signature(obj = "VegsoupData")}: ... }
    \item{as.character}{\code{signature(x = "VegsoupData")}: ... }
    \item{as.numeric}{\code{signature(x = "VegsoupData")}: ... }
    \item{coerce}{\code{signature(from = "VegsoupData", to = "list")}: ... }
    \item{colSums}{\code{signature(x = "VegsoupData")}: ... }
    \item{DecomposeNames}{\code{signature(obj = "VegsoupData")}: ... }
    \item{dim}{\code{signature(x = "VegsoupData")}: ... }
    \item{getDistconnected}{\code{signature(obj = "VegsoupData")}: ... }
    \item{head}{\code{signature(x = "VegsoupData")}: ... }
    \item{Indpower}{\code{signature(obj = "VegsoupData")}: ... }
    \item{Indspc}{\code{signature(obj = "VegsoupData")}: ... }
    \item{KML}{\code{signature(obj = "VegsoupData")}: ... }
    \item{Layers}{\code{signature(obj = "VegsoupData")}: ... }
    \item{names}{\code{signature(x = "VegsoupData")}: ... }
    \item{ncol}{\code{signature(x = "VegsoupData")}: ... }
    \item{nrow}{\code{signature(x = "VegsoupData")}: ... }
    \item{plot}{\code{signature(x = "VegsoupData", y = "missing")}: ... }
    \item{Richness}{\code{signature(obj = "VegsoupData")}: ... }
    \item{rownames}{\code{signature(x = "VegsoupData", do.NULL = "missing", prefix = "missing")}: ... }
    \item{rowSums}{\code{signature(x = "VegsoupData")}: ... }
    \item{SampleVegsoup}{\code{signature(x = "VegsoupData")}: ... }
    \item{Sites}{\code{signature(obj = "VegsoupData")}: ... }
    \item{Sites<-}{\code{signature(obj = "VegsoupData", value = "data.frame")}: ... }
    \item{Stride}{\code{signature(obj = "VegsoupData")}: ... }
    \item{summary}{\code{signature(object = "VegsoupData")}: ... }
    \item{tail}{\code{signature(x = "VegsoupData")}: ... }
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
showClass("VegsoupData")
}
\keyword{classes}
