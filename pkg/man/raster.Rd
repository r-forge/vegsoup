\name{extract}
\docType{methods}

\alias{extract}
\alias{extract,Vegsoup-method}
\alias{extract,Raster,Vegsoup-method}

\title{ Extract values from Raster objects }

\description{
Extract the values of the cells of a \code{Raster} object for the locations (\code{SpatialPoints}) of a \code{Vegsoup} object; that is perform a spatial query.
}

\usage{
\S4method{extract}{Vegsoup}(x, y, ...) 
}

\arguments{
  \item{ x }{
    A \code{Raster*} object.
  }
  \item{ y }{
    A \code{Vegsoup*} object.
  }
  \item{ \dots }{
    Additional arguments passed to \code{\link[raster:extract]{extract}}.
  }
}

\details{
  See the the documentation of \code{\link[raster:extract]{extract}} in \pkg{raster} for
  details and additional arguments.
}

\value{
  A vector for RasterLayer objects and a matrix for a RasterStack or RasterBrick object. A list   
  if a \code{buffer} argument is used or data.frame {\code{df=TRUE}}.
}

%%\seealso{
%%}

\author{ Roland Kaiser }

\examples{
require(raster)

# RasterLayer
r1 <- raster(ncol = 10, nrow = 10, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
r1[] <- sample(1:ncell(r1))

# 
data(sim100)
x <- sim100


plot(r1)
points(x)

extract(r1, x) # a vector

# RasterStack and RasterBrick
r2 <- raster(ncol = 10, nrow = 10, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
r2[] <- sample(1:ncell(r2))

s <- stack(r1, r2)
b <- brick(r1, r2)
extract(s, x) # a matrix
extract(b, x) # a matrix
}

\keyword{methods}
\keyword{spatial}

