\name{Vegsoup}
\alias{Vegsoup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an object of class vegsoup
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Vegsoup(x, y, z, scale = c("Braun-Blanquet", "Braun-Blanquet 2", "Domin", "frequency", "binary"),
group, sp.points, sp.polygons, proj4string = "+proj=longlat +datum=WGS84")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Species data.frame.
}
  \item{y}{
Sites data.frame.
}
  \item{z}{
Taxonomy data.frame.
}
  \item{scale}{
Abundance scale, Possible choices are "Braun-Blanquet" "frequency" "binary"
}
  \item{group}{
Apriori grouping, if any and can be missing.
}
  \item{proj4string}{
projection attributes for slots \code{sp.points} and \code{sp.polygons}. If missing set to \code{CRS("+init=epsg4326")}) (see \code{\link{proj4string}}).
}
  \item{sp.points}{
Object of class \code{\link{SpatialPointsDataFrame}}, if missing and coordinate information can not be extracted from \code{y}, a random point pattern is generated.
}
  \item{sp.polygons}{
Object of class \code{\link{SpatialPolygonsDataFrame}}, if missing random polygon pattern is generated based on sp.points.
}
  \item{col.names}{
List with three components of mode character indicating which columns to select from \code{x}, \code{y} and \code{z}, respectively.
}
  \item{verbose}{
Show status reports during function call.
}
}
\details{
\code{Vegsoup()} is the constructor function to create a valid object of class \code{Vegsoup}. Argument \code{x} should conform to the following format. Column names \code{plot}, \code{abbr}, \code{layer} and \code{cov}. Argument \code{y} should have names \code{plot}, \code{variable} and \code{value}. Finally, \code{z} needs to have column names \code{abbr} and \code{taxon}. The first one matching exactly the strings in \code{x\$abbr}.

col.names must be a list of character and length 3.
The function calls \code{\link{make.names}} an the species identifier (columns \code{abbr}, if not specified). This will for example add a dot for blanks. Using \code{make.names} ensures that a syntactically valid character vector can be maintained. This is crucial to maintain a primary key to the inhernet taxonomy lookup table.

 If an object returned from function \link{QueryTaxonomy} or an object that has a list element named \code{taxonomy} is used as argument for \code{z} list element \code{"taxonomy"} is used. If column names of argumnets \code{x, y, z} do not agree with the previous coneventions a list naming the columns to has has to be supplied 

If the sites data has variables exactly named longitude and latitude those will be used to extract coordinates. Coordinates have to be formatted as decimal degrees! See e.g. \code{\link{char2dms}} to convert between formats. \code{vegsoup} will discard any alphanumeric characters by calling \code{gsub(pattern = "[[:alpha:]]", replacement = "", ...}. Decimals ("," and ".") are handled to yield numeric values for columns that can be coerced to \code{as.numeric} (e.\,g. in x\$cov if abundance is not ordinal, or for all in y that represent continuous measurement on plot.).
}
\note{

}

\section{Warning}{

The function calls \code{as.data.frame(as.matrix(...), stringsAsFactors = FALSE)} on objects \code{x}, \code{y}, and \code{z} for safety and to ensure validity. When suppling \code{col.names} ensure that these will match exactly.

All missing values that might result when casting \code{y} will be replaced with zeros, not \code{NA}!

If sp.points and/or sp.polygons are supplied the data slot (\code{SpatialPointsDataFrame@data}) must have a column named plot. In addition, this column has to be ordered. Sorting can be performed using \code{SpatialPointsDataFrame[order(SpatialPointsDataFrame\$plot), ]}. The same is mandatory of slot \code{sp.polygons}.

When suppling argument \code{proj4string} please ensure that \code{CRS(proj4string)} works.
}
\value{
An object of class Vegsoup, see class \link{Vegsoup-class}.
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
\examples{
library(vegsoup)
data(testdata)

qry <- Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet"))


	
}
\seealso{
\code{\link{Vegsoup-class}}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
