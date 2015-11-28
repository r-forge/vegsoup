\encoding{UTF-8}

\name{map}
\docType{methods}

\alias{quicklook}
\alias{quicklook,Vegsoup-method}

\alias{ellipsoidhull}
\alias{ellipsoidhull,VegsoupPartition-method}

\title{ Draw Map of Plot Locations }

\description{
\code{map} draws a simple vector background map and plot locations. \code{ellipsoidhull} computes the \emph{ellipsoid hull} or \emph{spanning ellipsoid} such that all given points lie just inside or on the boundary of the ellipsoid and adds it to a map.

\code{quicklook} opens a browser window and plots pushpins of plot coordiantes on top of \sQuote{Google Maps}.
}

\usage{
\S4method{map}{Vegsoup}(x, database, ...)
\S4method{quicklook}{Vegsoup}(x)
\S4method{ellipsoidhull}{VegsoupPartition}(x, tol = 0.01, maxit = 5000, ret.wt = FALSE,
  ret.sqdist = FALSE, ret.pr = FALSE)
}

\arguments{
  \item{ x }{
	\code{Vegsoup*} or for ellipsoidhull a \code{'VegsoupPartition'} object.
  }
  \item{ database }{
	Character string naming a geographical database. See \code{\link[maps:map]{map}}.
   }
  \item{ \dots }{
	Arguments passed to \code{\link[maps]{map}} in package \pkg{maps}.
  }
  \item{ tol, maxit, ret.wt, ret.sqdist, ret.pr }{
	Arguments to \code{\link[cluster]{ellipsoidhull}} in package
	\pkg{cluster}.
  }
}

\details{
\code{ellipsoidhull} is a simple wrapper for function \code{\link{ellipsoidhull}} in package cluster. The main purpose of the function is to visualize partitions on a map.

Function \code{quicklook} opens a browser window and plots pushpins of plot centers on top of \sQuote{Google Maps}. See \code{\link[googleVis]{gvisMap}} in package \pkg{googleVis} for details.
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
  \code{\link[googleVis]{gvisMap}} in package \pkg{googleVis},
  \code{\link{ellipsoidhull}} in package \pkg{cluster}
}
\keyword{ methods }
\keyword{ hplot }

