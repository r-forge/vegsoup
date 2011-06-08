\name{VegsoupDataPartition-class}
\Rdversion{1.1}
\docType{class}
\alias{VegsoupDataPartition-class}
\alias{[,VegsoupDataPartition,ANY,ANY,missing-method}
\alias{Confus,VegsoupDataPartition,VegsoupDataPartition-method}
\alias{Constancy,VegsoupDataPartition-method}
\alias{Contingency,VegsoupDataPartition-method}
\alias{Disdiam,VegsoupDataPartition-method}
\alias{Fidelity,VegsoupDataPartition-method}
\alias{FisherTest,VegsoupDataPartition-method}
\alias{getDist,VegsoupDataPartition-method}
\alias{getDistconnected,VegsoupDataPartition-method}
\alias{getK,VegsoupDataPartition-method}
\alias{head,VegsoupDataPartition-method}
\alias{Indval,VegsoupDataPartition-method}
\alias{Isamic,VegsoupDataPartition-method}
\alias{Latex,VegsoupDataPartition-method}
\alias{Multipatt,VegsoupDataPartition-method}
\alias{Murdoch,VegsoupDataPartition-method}
\alias{Optindval,VegsoupDataPartition-method}
\alias{Optsil,VegsoupDataPartition-method}
\alias{Partana,VegsoupDataPartition-method}
\alias{Partitioning,VegsoupDataPartition-method}
\alias{PartitioningMatrix,VegsoupDataPartition-method}
\alias{Phi,VegsoupDataPartition-method}
\alias{plot,VegsoupDataPartition,missing-method}
\alias{SigFidelity,VegsoupDataPartition-method}
\alias{Silhouette,VegsoupDataPartition-method}
\alias{Spread,VegsoupDataPartition-method}
\alias{summary,VegsoupDataPartition-method}
\alias{Tabdev,VegsoupDataPartition-method}
\alias{Typal,VegsoupDataPartition-method}

\title{Class "VegsoupDataPartition"}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("VegsoupDataPartition", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{part}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{method}:}{Object of class \code{"character"} ~~ }
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
Class \code{"\linkS4class{VegsoupData}"}, directly.
Class \code{"\linkS4class{Vegsoup}"}, by class "VegsoupData", distance 2.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "VegsoupDataPartition", i = "ANY", j = "ANY", drop = "missing")}: ... }
    \item{Confus}{\code{signature(obj1 = "VegsoupDataPartition", obj2 = "VegsoupDataPartition")}: ... }
    \item{Constancy}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Contingency}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Disdiam}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Fidelity}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{FisherTest}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{getDist}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{getDistconnected}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{getK}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{head}{\code{signature(x = "VegsoupDataPartition")}: ... }
    \item{Indval}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Isamic}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Latex}{\code{signature(object = "VegsoupDataPartition")}: ... }
    \item{Multipatt}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Murdoch}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Optindval}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Optsil}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Partana}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Partitioning}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{PartitioningMatrix}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Phi}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{plot}{\code{signature(x = "VegsoupDataPartition", y = "missing")}: ... }
    \item{SigFidelity}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Silhouette}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Spread}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{summary}{\code{signature(object = "VegsoupDataPartition")}: ... }
    \item{Tabdev}{\code{signature(obj = "VegsoupDataPartition")}: ... }
    \item{Typal}{\code{signature(obj = "VegsoupDataPartition")}: ... }
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
showClass("VegsoupDataPartition")
}
\keyword{classes}
