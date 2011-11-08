\name{Shp2SitesLong}
\alias{Shp2SitesLong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract coordinates from OGR data source.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Shp2SitesLong(dsn, layer, plot.column, elevation.column, round = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{dsn}{data source name (folder where ESRI Shapefile is stored).}
\item{layer}{layer name (filename of the ESRI Shapefile).}
\item{plot.column}{character string giving the name of the column in the ESRI Shapefile attribute table that identifies unique plots (Not case sensitive! See details.)
}
\item{elevation.column}{character string giving the name of the column in the ESRI Shapefile attribute table that identifies altitude measurements (Not case sensitive! see details!). If missing and the geometry is 3D the function read the third dimension.}
\item{round}{integer round decimal places to given precision (see details).}
\item{round}{print some message on screen.}
}
\details{
If the supplied ESRI Shapefile is 3d, the third dimension is used as altitude. If some values are missing those will not raise NA's but will set to zero elevation. 
Arguments plot.column and elevation are forced to lower case using function to.lower(). The same applies to column names in the dbf-file associated with the ESRI-Shapefile.
Using WGS decimal coordinates, 6 decimals places are sufficient for a precision at the meter level.
}
\value{
Returns an object of class \code{"data.frame"} with columns 'plot', 'variable' and 'value' conforming vegsoup's raw data model.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{VegsoupData-class}, \link{readOGR}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
