\name{map}
\docType{methods}
\alias{map}
\alias{map-methods}
\alias{ellipsoidhull}
\alias{ellipsoidhull-methods}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw Map of Plot Locations
%%Compute the Ellipsoid Hull of Plot Locations Grouped by Partitions
}
\description{
\code{map} draws a simple vector background map and plot locations. \code{ellipsoidhull} computes the \emph{ellipsoid hull} or \emph{spanning ellipsoid} such that all given points lie just inside or on the boundary of the ellipsoid and adds it to a map.
}
\usage{
map(obj, ...)
ellipsoidhull(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
    an object inhertiting from class \code{"VegsoupDataPartition"}
}
  \item{col}{
    color of the lines
}
  \item{...}{
    arguments passed to \code{\link[maps:map]{map}} in package \pkg{maps}, or \code{ellipsoidhull} in package \pkg{cluster}.
}  
}
\details{
method \code{map} is intended to provide a quick and dirty map background to dispaly plot locations.

\code{ellipsoidhull} is a simple wrapper for function \code{\link{ellipsoidhull}} in package cluster. The main purpose of the function is to visualize partitions on a map.
}
\value{
\code{map} creates a map plot.

\code{ellipsoidhull} adds ellipses to the current plot. The function returns a list of objects of class \code{"ellipsoid"} for all partitions having 3 ore more points (plots), otherwise a matrix of ploting coordinates representing each plot location. See \code{\link[cluster:ellipsoidhull]{ellipsoidhull}} for details.
  
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
\code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
\keyword{ hplot }
