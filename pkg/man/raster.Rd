\encoding{UTF-8}

\name{extract}
\docType{methods}

\alias{extract}
\alias{extract,Vegsoup-method}
\alias{extract,SpatRaster,Vegsoup-method}

\title{ Extract values from Raster objects }

\description{
  Extract the values of the cells of a \code{"SpatRaster"} object for the
  locations (\code{SpatialPoints}) of a \verb{Vegsoup} object; that is perform
  a spatial query.
}

\usage{
\S4method{extract}{Vegsoup}(x, y, ...) 
}

\arguments{
  \item{ x }{
	A \code{SpatRaster} object.
  }
  \item{ y }{
	A \code{Vegsoup*} object.
  }
  \item{ \dots }{
	Additional arguments passed to \code{\link[terra:extract]{extract}}.
  }
}

\details{
  See the the documentation of \code{\link[terra:extract]{extract}} in
  \pkg{terra} for details and additional arguments.
}

\value{
  A vector for a single band SpatRaster object and a matrix for a
  multi band SpatRaster object. A list if a \code{buffer} argument
  is used or data.frame {\code{df=TRUE}}.
}

%%\seealso{
%%}

\author{ Roland Kaiser }

\examples{
require(terra)

# SpatRaster
r1 <- rast(ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
r1[] <- sample(1:ncell(r1))

x <- coenoflex()

plot(r1)
points(x)

extract(r1, x) # a vector

# SpatRaster with 2 layers
r2 <- rast(ncol = 10, nrow = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
r2[] <- sample(1:ncell(r2))

s <- c(r1, r2)
extract(s, x) # a matrix

}

\keyword{methods}
\keyword{spatial}

