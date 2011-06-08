\name{Vegsoup-class}
\Rdversion{1.1}
\docType{class}
\alias{Vegsoup-class}
\alias{Abbreviation,Vegsoup-method}
\alias{Abbreviation<-,Vegsoup,character-method}
\alias{AbundanceScale,Vegsoup-method}
\alias{AbundanceScale<-,Vegsoup,list-method}
\alias{AprioriGrouping,Vegsoup-method}
\alias{Layers,Vegsoup-method}
\alias{Layers<-,Vegsoup-method}
\alias{plot,Vegsoup,missing-method}
\alias{SitesLong,Vegsoup-method}
\alias{SitesLong<-,Vegsoup,data.frame-method}
\alias{SpatialPointsVegsoup,Vegsoup-method}
\alias{SpatialPointsVegsoup<-,Vegsoup,SpatialPointsDataFrame-method}
\alias{SpatialPolygonsVegsoup,Vegsoup-method}
\alias{SpatialPolygonsVegsoup<-,Vegsoup,SpatialPolygonsDataFrame-method}
\alias{SpeciesLong,Vegsoup-method}
\alias{SpeciesLong<-,Vegsoup,data.frame-method}
\alias{Taxonomy,Vegsoup-method}
\alias{Taxonomy<-,Vegsoup,data.frame-method}

\title{Class "Vegsoup"}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Vegsoup", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
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
\section{Methods}{
  \describe{
    \item{Abbreviation}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Abbreviation<-}{\code{signature(obj = "Vegsoup", value = "character")}: ... }
    \item{AbundanceScale}{\code{signature(obj = "Vegsoup")}: ... }
    \item{AbundanceScale<-}{\code{signature(obj = "Vegsoup", value = "list")}: ... }
    \item{AprioriGrouping}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Layers}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Layers<-}{\code{signature(obj = "Vegsoup")}: ... }
    \item{plot}{\code{signature(x = "Vegsoup", y = "missing")}: ... }
    \item{SitesLong}{\code{signature(obj = "Vegsoup")}: ... }
    \item{SitesLong<-}{\code{signature(obj = "Vegsoup", value = "data.frame")}: ... }
    \item{SpatialPointsVegsoup}{\code{signature(obj = "Vegsoup")}: ... }
    \item{SpatialPointsVegsoup<-}{\code{signature(obj = "Vegsoup", value = "SpatialPointsDataFrame")}: ... }
    \item{SpatialPolygonsVegsoup}{\code{signature(obj = "Vegsoup")}: ... }
    \item{SpatialPolygonsVegsoup<-}{\code{signature(obj = "Vegsoup", value = "SpatialPolygonsDataFrame")}: ... }
    \item{SpeciesLong}{\code{signature(obj = "Vegsoup")}: ... }
    \item{SpeciesLong<-}{\code{signature(obj = "Vegsoup", value = "data.frame")}: ... }
    \item{Taxonomy}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Taxonomy<-}{\code{signature(obj = "Vegsoup", value = "data.frame")}: ... }
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
showClass("Vegsoup")
}
\keyword{classes}
