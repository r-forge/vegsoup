\name{spatial}
\docType{methods}

\alias{coordinates}
\alias{coordinates,Vegsoup-method}

\alias{coordinates<-}
\alias{coordinates<-,Vegsoup-method}
\alias{coordinates<-,Vegsoup,ANY-method}
\alias{coordinates<-,Vegsoup,CRS-method}

\alias{bbox}
\alias{bbox,Vegsoup-method}

\alias{proj4string}
\alias{proj4string,Vegsoup-method}

\alias{proj4string<-}
\alias{proj4string<-,Vegsoup-method}
\alias{proj4string<-,Vegsoup,CRS-method}

\alias{spTransform}
\alias{spTransform,Vegsoup-method}

\alias{SpatialPointsVegsoup}
\alias{SpatialPointsVegsoup,Vegsoup-method}

\alias{SpatialPolygonsVegsoup}
\alias{SpatialPolygonsVegsoup,Vegsoup-method}

\title{ Spatial Methods for \code{Vegsoup*} Objects } 

\description{
Spatial methods as defined in package \pkg{sp}.
}

\usage{
\S4method{coordinates}{Vegsoup}(obj)

\S4method{coordinates}{Vegsoup}(obj) <- value

\S4method{bbox}{Vegsoup}(obj)

\S4method{proj4string}{Vegsoup}(obj)

\S4method{proj4string}{Vegsoup}(obj) <- value

\S4method{spTransform}{Vegsoup}(obj, CRS)

\S4method{SpatialPointsVegsoup}{Vegsoup}(obj)

\S4method{SpatialPolygonsVegsoup}{Vegsoup}(obj)
}

\arguments{
  \item{ obj }{
    an object of class \code{Vegsoup*},
  }
  \item{ value }{
    For \code{coordinates} spatial coordinates; either a \code{"matrix"},
    \code{"list"}, or \code{"data.frame"} with numeric data. See
    \code{\link[sp:coordinates]{coordinates}}.

    For \code{proj4string} a \code{\linkS4class{CRS}} object, containing a
    valid Proj.4 string. See \code{\link[sp:proj4string]{proj4string}}.
  }
  \item{ CRS }{
    An object of class \code{"CRS"}. See \code{\linkS4class{CRS}}.
  }
  \item{ \dots }{
    Additional arguments, not used.
  }
}

\details{
\code{coordinates} retrieves or sets the spatial coordinates.

\code{bbox} retrieves the spatial bounding box from \code{coordinates(obj)}

\code{proj4string} retrieves or sets the projection attributes.

\code{spTransform} provides transformation between datum(s) and conversion between projections (also known as projection and/or re-projection).

All methods operate on the spatial slots of an object: \code{SpatialPointsVegsoup} and \code{SpatialPolygonsVegsoup}.
}

\value{
An object depending on the input class.
}

\author{ Roladn Kaiser }

\seealso{
\code{\link{Spatial}}
}
\keyword{ methods }

