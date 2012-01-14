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
Vegsoup(x, y, z, scale = c("Braun-Blanquet", "frequency", "binary"), group, sp.points, sp.polygons, proj4string = "+proj=longlat +datum=WGS84")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Species data.frame
}
  \item{y}{Sites data.frame}
  \item{z}{Taxonomy data.frame}
  \item{scale}{Abundance scale, Possible choices are "Braun-Blanquet" "frequency" "binary"}
  \item{group}{Apriori grouping, if any}
  \item{proj4string}{projection attributes for slots sp.points and sp.polygons (see \code{\link{proj4string}}) }
  \item{sp.points}{Object of class \code{\link{SpatialPointsDataFrame}}, if missing random point pattern is generated}
  \item{sp.polygons}{Object of class \code{\link{SpatialPolygonsDataFrame}}, if missing random polygon pattern is generated based on sp.points }
}
\details{
Constructor function to create an object of class Vegsoup. x should conform to the following format. Column names plot, abbr, layer, cov. x should have names plot, variable, value. If an object returned from function \link{QueryTaxonomy} or an object that has a list element named \code{taxonomy} is used as argument for \code{z} list element \code{$taxonomy} is used. If the sites data has variables exactly named longitude and latitude those will be used to gather coordinates. Coordinates have to be formatted as decimal degrees! See e.g. \link{char2dms} to convert between formats. Any alphanumeric characters will be discarded using \code{gsub(pattern = "[[:alpha:]]", replacement = "", ...}. Decimals ("," and ".") are handled to yield numeric values for column \code{x$cov}.
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

\seealso{
See Also \code{\link{Vegsoup-class}}, ~~~

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
