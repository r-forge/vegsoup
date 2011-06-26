\name{Shp2SitesLong}
\alias{Shp2SitesLong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract coordinates from ESRI Shapefile.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Shp2SitesLong(dsn, layer, plot.column, elevation.column, round = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{
data source name (folder where ESRI Shapefiles are stored)
}
  \item{layer}{
layer name (filename of the ESRI Shapefiles) 
}
  \item{plot.column}{
character string giving the name of the column in the ESRI Shapefile attribute table that identifies unique plots (Not case sensitive! See details!)
}
  \item{elevation.column}{
character string giving the name of the column in the ESRI Shapefile attribute table that identifies altitude measurements (Not case sensitive! see details!). If missing and the geometry is 3d the function read the third dimension.
}
  \item{round}{
integer round decimal places to given precision (See details!)
}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dsn, layer, plot.column, elevation.column, round = TRUE) {

if (missing(plot.column)) {
	stop ("please supply column name indicating plot ids in dbf file")
  }

require(rgdal)
pt <- readOGR(dsn, layer)
names(pt) <- tolower(names(pt))
pt <- spTransform(pt, CRS("+init=epsg:4326"))

if (missing(elevation.column)) {
	df <- data.frame(coordinates(pt),
		as.character(pt@data[,names(pt) == tolower(plot.column)]),
		stringsAsFactors = FALSE)
  } else {
	df <- data.frame(coordinates(pt),
		as.numeric(as.character(pt@data[,names(pt) == tolower(elevation.column)])),
		as.character(pt@data[,names(pt) == tolower(plot.column)]),
		stringsAsFactors = FALSE)
  }

if (dim(coordinates(pt))[2] == 2 & missing(elevation.column)) {
	
	names(df)[1:3] <- c("longitude", "latitude", "plot")
	res <- data.frame(as.character(df$plot), stack(df),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	names(res) <- c("plot", "variable", "value")
	res$value[res$variable == "longitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "latitude"] <-
		round(res$value[res$variable == "longitude"], 6)

  }

if (dim(coordinates(pt))[2] == 2 & !missing(elevation.column)) {
	names(df)[1:4] <- c("longitude", "latitude", "altitude", "plot")

	res <- data.frame(as.character(df$plot),
		stack(df, select = 1:3),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	res[,3] <- as.numeric(as.character(res[,3]))
	names(res) <- c("plot", "variable", "value")
	res$value[res$variable == "longitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "latitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "altitude"] <-
		round(res$value[res$variable == "altitude"], 0)

  }

if (dim(coordinates(pt))[2] == 3) {
	names(df)[1:4] <- c("longitude", "latitude", "altitude", "plot")
	res <- data.frame(as.character(df$plot), stack(df, select = 1:3),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	names(res) <- c("plot", "variable", "value")
	res$value[res$variable == "longitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "latitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "altitude"] <-
		round(res$value[res$variable == "altitude"], 0)

  }

return(invisible(res))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
