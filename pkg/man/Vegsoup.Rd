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
Vegsoup(x, y, z, scale = c("Braun-Blanquet", "frequency", "binary"), group, sp.points, sp.polygons)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Species data.frame
}
  \item{y}{
Sites data.frame
}
  \item{z}{
Taxonomy data.frame
}
  \item{scale}{
Abundance scale
}
  \item{group}{
Apriori grouping, if any
}
  \item{sp.points}{
Object of class SpatialPointsDataFrame
}
  \item{sp.polygons}{
Object of class SpatialPolygonsDataFrame
}
}
\details{
Constructor function to create an object of class Vegsoup. x should conform to the following format. Column names plot, abbr, layer, cov. x should have names plot, variable, value. If an object returned from function \link{QueryTaxonomy} or an object that has a list element named \code{taxonomy} is used as argument for \code{z} element \code{$taxonomy} is used.
}
\value{
An object of class Vegsoup.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
%% ValidateVegsoup.R
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
