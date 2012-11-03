\name{EllipsoidHull}
\alias{EllipsoidHull}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the Ellipsoid Hull of Plot Locations Grouped by Partitons
}
\description{
Compute the 'ellipsoid hull' or 'panning ellipsoid' such that all given points lie just inside or on the boundary of the ellipsoid.
}
\usage{
EllipsoidHull(obj, add = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
    an object inhertiting from class \code{"VegsoupDataPartition"}
}
  \item{add}{
    add to existing plot  	
}
  \item{col}{
    color of the lines
}
  \item{...}{
    arguments passed to \code{ellipsoidhull} in package cluster.
}  
}
\details{
A simple wrapper for function \code{\link{ellipsoidhull}} in package cluster. The main purpose of the function is to visualize partitions on a map.
}
\value{
returns a list of objects of class \code{"ellipsoid"} for all partitions having 3 ore more plots, otherwise a matrix of ploting coordinates represting each plot location. See \code{\link{ellipsoidhull}} for details.
  
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
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
