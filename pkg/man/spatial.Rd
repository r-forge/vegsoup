\name{sp-methods}
\docType{methods}
\alias{ccordinates}
\alias{coordinates-methods}
\alias{coordinates,VegsoupData-method}

\alias{SpatialPointsVegsoup}
\alias{SpatialPointsVegsoup-methods}
\alias{SpatialPointsVegsoup,Vegsoup-method}
\alias{SpatialPointsVegsoup<-}
\alias{SpatialPointsVegsoup<--methods}
\alias{SpatialPointsVegsoup<-,Vegsoup,SpatialPointsDataFrame-method}

\alias{SpatialPolygonsVegsoup}
\alias{SpatialPolygonsVegsoup-methods}
\alias{SpatialPolygonsVegsoup,Vegsoup-method}
\alias{SpatialPolygonsVegsoup<-}
\alias{SpatialPolygonsVegsoup<--methods}
\alias{SpatialPolygonsVegsoup<-,Vegsoup,SpatialPolygonsDataFrame-method}



\title{ 'sp' and 'rgdal' Methods for Objects of Class VegsoupData*  } 


\description{
Methods 
}

\usage{
coordinates(obj, ...)
coordinates(obj) <- value

bbox(obj)

proj4string(obj)
proj4string(obj) <- value

spTransform(obj, CRS)

SpatialPointsVegsoup(obj)
SpatialPolygonsVegsoup(obj)
}
\arguments{
  \item{obj}{
an object of class \code{VegsoupData*},
}
  \item{value}{
For \code{coordinates} spatial coordinates; either a \code{matrix}, \code{list}, or \code{data.frame} with numeric data. See \code{\link[sp:coordinates]{coordinates}}

For \code{proj4string} a CRS object, containing a valid proj4 string. See \code{\link[sp:proj4string]{proj4string}}.
}
  \item{ CRS }{
An object of class \code{"CRS"}. See \code{\link[rgdal:CRS]{CRS}}.
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

\seealso{
\code{\link{Spatial}},\code{\link{SpatialPointsDataFrame}}
}
\keyword{ methods }

