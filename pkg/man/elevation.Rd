\name{elevation}

\docType{methods}

\alias{elevation}
\alias{elevation,Vegsoup-method}

\title{ Query Elevation }

\description{
Get topographic height information from the SRTM3 data set using package \pkg{geonames} and assign a column named \code{'elevation'} to the sites data. If such a column name already exists, the method append a column named \code{'elevation.srtm'} to prevent overwriting of already existing columns.
}

\details{
If the SRTM3 data set has missing values a zero elevation is returned for that coordiante.
}

\author{ Roland Kaiser }

\seealso{
\code{\link{extract}},\code{\link[geonames:GNsrtm3]{GNsrtm3}} in package \pkg{geonames}.
}

\note{
Depending of the size of the input object (number of plots), the query to the \href{www.geonames.org}{genonames} server may take a while to complete?
}

\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# fails with package check?
# dta.srtm <- elevation(dta)
}
\keyword{ methods }