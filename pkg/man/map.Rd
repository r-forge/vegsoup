\name{map}
\docType{methods}
\alias{map}
\alias{map-methods}
\alias{QuickMap}
\alias{QuickMap-methods}
\alias{ellipsoidhull}
\alias{ellipsoidhull-methods}

\title{ Draw Map of Plot Locations }

\description{
\code{map} draws a simple vector background map and plot locations. \code{ellipsoidhull} computes the \emph{ellipsoid hull} or \emph{spanning ellipsoid} such that all given points lie just inside or on the boundary of the ellipsoid and adds it to a map.

\code{QuickMap} opens a browser window and plots pushpins of plot centers on top of "Google Maps"
}
\usage{
## S4 method for signature 'Vegsoup*'
map(obj, ...)
## S4 method for signature 'VegsoupPartition*'
ellipsoidhull(x, ...)
}

\arguments{
  \item{ obj }{
    a \code{Vegsoup*} object.
  }
  \item{ x }{
    an object inhertiting from class \code{"VegsoupPartition"}.
  }
  \item{ col }{
    Color of the lines.
   }
  \item{ \dots }{
    Arguments passed to \code{\link[maps:map]{map}} in package \pkg{maps},
    or \code{\link[cluster:ellipsoidhull]{ellipsoidhull}} in package
    \pkg{cluster}.
  }
}
\details{
method \code{map} is intended to provide a quick and dirty map background to dispaly plot locations.

\code{ellipsoidhull} is a simple wrapper for function \code{\link{ellipsoidhull}} in package cluster. The main purpose of the function is to visualize partitions on a map.

Function \code{QuickMap} opens a browser window and plots pushpins of plot centers on top of "Google Maps". See \link{gvisMap} in package \pkg{googleVis} for details.
}
\value{
  \code{map} creates a map plot.

  \code{ellipsoidhull} adds ellipses to the current plot. The function returns
  a list of objects of class \code{"ellipsoid"} for all partitions having 3
  ore more points (plots), otherwise a matrix of ploting coordinates
  representing each plot location. See
  \code{\link[cluster:ellipsoidhull]{ellipsoidhull}} for details.
  
}
\author{ Roland Kaiser }
\seealso{
  \code{\link{gvisMap}} in package \pkg{googleVis},
  \code{\link{ellipsoidhull}} in package \pkg{cluster}
}
\keyword{ methods }
\keyword{ hplot }

