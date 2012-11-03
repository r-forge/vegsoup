\name{Shared}
\alias{Shared}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Shared Species between Partitions
}
\description{
Computes a diagonal matrix showing percentages of shared species between partitons.
}
\usage{
Shared(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
an object inheriting from class \code{VegsoupDataPartition}.
}
}
\details{
The computed index is this equal to \code{designdist(t(Constancy(prt) > 0), method = "J/(A+B)*100")} and simply returns the percentage of shared species between partitions. See \code{\link{designdist}} for details on the \code{method} argument.
}
\value{
	an object of class \code{\link{dist}}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
