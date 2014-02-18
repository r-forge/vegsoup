\name{elevation}

\docType{methods}

\alias{elevation}
\alias{elevation,Vegsoup-method}

\title{ Query Elevation }

\description{
Get topographic height information from the SRTM3 data set using package \pkg{geonames} and assign a column named \code{'elevation'} to the sites data. If such a column name already exists, the method append a column named \code{'elevation.srtm'} to prevent overwriting of already existing columns.
}

\details{
If the SRTM3 data set has missing values a zero elevation is returned for that coordinate.
}

\author{ Roland Kaiser }

\seealso{
\code{\link{extract}},\code{\link[geonames:GNsrtm3]{GNsrtm3}} in package \pkg{geonames}.
}

\note{
Depending of the size of the input object (number of plots), the query to the \href{www.geonames.org}{genonames} server may take a while to complete?
}

\examples{
require(vegsoup)

data(berchtesgaden)
x <- berchtesgaden

# fails with package check?
\dontrun{
x.srtm <- elevation(x)
}
}
\keyword{ methods }

