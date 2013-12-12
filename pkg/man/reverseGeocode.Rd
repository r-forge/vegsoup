\name{reverseGeocode}
\alias{reverseGeocode}

\title{ Reverse geocoding }

\description{
Performs reverse geocoding using function \code{\link[ggmap:revgeocode]{revgeocode}} of package \pkg{ggmap}.
}

\usage{
\S4method{reverseGeocode}{Vegsoup}(x, ...)

\S4method{reverseGeocode}{SpatialPointsDataFrame}(x, ...)
}

\arguments{
  \item{ x }{
A vegsoup or SpatialPointsDataFrame object.
}
  \item{ \dots }{
not used.
}
}

\details{
The function returns a formatted string of coordinates including elevation (as returned by \code{\link[geonames:GNsrtm3]{GNsrtm3}} of package \pkg{geonames}.) and a locality description based on administrative units. The SpatialPointsDataFrame method seeks variables \code{"accuracy"} or \code{"precision"} from the object.
}

\value{
A modified vegsoup object with two additional columns \code{"coordinate.string"} and \code{"locality"} or a matrix holding the former variables.
}

\author{
Roland Kaiser
}

\note{
Projection is assumed to be longitude/latitude (e.g. \code{CRS("+init=epsg:4326")}). Use \code{\link{spTransform}} for reprojection if necessary.
}

\section{Warning}{
Make sure that the object has a column named \code{"horizontal.precision"} in order to obtain coordinates uncertainty. The method will overwrite existing columns named \code{"coordinate.string"} and \code{"locality"} if already present.
}

\seealso{
\code{\link{coordinates}}
}
\examples{
#	Nothing here yet
}

\keyword{ spatial }
