\encoding{UTF-8}

\name{spatial}
\docType{methods}

\alias{coordinates}
\alias{coordinates,Vegsoup-method}
\alias{coordinates,Sites-method}

\alias{coordinates<-}
\alias{coordinates<-,Vegsoup-method}
\alias{coordinates<-,Vegsoup,ANY-method}
\alias{coordinates<-,Vegsoup,CRS-method}

\alias{bbox}
\alias{bbox,Vegsoup-method}

\alias{extent}
\alias{extent,Vegsoup-method}

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

\alias{over}
\alias{over,Vegsoup-method}
\alias{over,Vegsoup,SpatialPolygonsDataFrame-method}

\title{ Spatial Methods for \code{Vegsoup*} Objects } 

\description{
Spatial methods as defined in package \pkg{sp}.
}

\usage{
\S4method{coordinates}{Vegsoup}(obj)
\S4method{coordinates}{Sites}(obj)

\S4method{coordinates}{Vegsoup}(obj) <- value

\S4method{bbox}{Vegsoup}(obj)

\S4method{bbox}{Vegsoup}(x)

\S4method{proj4string}{Vegsoup}(obj)

\S4method{proj4string}{Vegsoup}(obj) <- value

\S4method{spTransform}{Vegsoup}(obj, CRS)

\S4method{SpatialPointsVegsoup}{Vegsoup}(obj)

\S4method{SpatialPolygonsVegsoup}{Vegsoup}(obj)

\S4method{over}{Vegsoup}(x, y, returnList = FALSE, fn = NULL, ...)
}

\arguments{
  \item{ x, obj }{
    \code{Vegsoup} object.
  }
  \item{ y }{
    \code{SpatialPolygonsDataFrame} object.
  }
  \item{ value }{
    For \code{coordinates} spatial coordinates; either a \code{"matrix"},
    \code{"list"}, or \code{"data.frame"} with numeric data. See
    \code{\link[sp:coordinates]{coordinates}}.

    For \code{proj4string} a \code{\linkS4class{CRS}} object, containing a
    valid \emph{Proj.4} string.
  }
  \item{ CRS }{
    An object of class \code{"CRS"}. See \code{\linkS4class{CRS}}.
  }
  \item{ \dots }{
    Additional arguments, not used.
  }
  \item{ returnList, fn }{
    See \code{\link{over}} in \pkg{sp}.
  }
}

\details{
All methods operate on the spatial slots of an object:
\code{SpatialPointsVegsoup} and \code{SpatialPolygonsVegsoup}.

\itemize{
  \item
  \code{coordinates} retrieves or sets the spatial coordinates. Note, in
  contrast to the generic method defined in package \pkg{sp} it is possible
  to set coordinates, where they have already been set (accounts for class
  Vegsoup only)! The coordinates method for class \code{\linkS4class{Sites}}
  tries to get coordinates from named variables longitude and latitude,
  otherwise simulated coordinates in the unit square
  (\code{runif(length(unique(obj$plot)), min = 0, max = 1)}) are returned.

  \item
  \code{bbox} retrieves the spatial extent (extremes) from \code{coordinates(obj)}.

  \item
  \code{extent} returns the same but as class \code{\linkS4class{Extent}}.

  \item
  \code{proj4string} retrieves or sets the projection attributes (coordinate
  reference system). See \code{\link[sp:proj4string]{proj4string}} and
  \code{\linkS4class{CRS}} for details.

  \item
  \code{spTransform} provides transformation between datum(s) and conversion between
  projections (also known as projection and/or re-projection).

  \item
  \code{over} provides an overlay method for spatial polygons.
}
}

\value{
An object depending on the input class.
}

\author{ Roland Kaiser }

\seealso{
\code{\link{Spatial}},\code{\link{extract}}
}

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

# coordinate refernece system of data set is WGS 84
proj4string(x)

# transform to WGS 84 / Pseudo Mercator
xt <- spTransform(x, CRS("+init=epsg:3857"))

# plain matrix of coordiantes
coordinates(x)

# bounding box, as defined in package 'sp'
bbox(x)

# extent, the same as above but returned as class 'Extent'
# as defined in package 'raster'
extent(x)

# assign new coordinates
# first create the data as columns
x$X <- rnorm(nrow(x))
x$Y <- rnorm(nrow(x))
Sites(x)

# use the formula interface
coordinates(x) <- ~X+Y
coordinates(x)

# get slot 'sp.points' as sp class
class(SpatialPointsVegsoup(x))

# get slot 'sp.polygons' as sp class
class(SpatialPolygonsVegsoup(x))
}
\keyword{ methods }

