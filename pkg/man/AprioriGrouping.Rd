\name{AprioriGrouping}
\alias{AprioriGrouping}

\title{
Get Grouping Variable of Object Extending Class Vegsoup
}
\description{
Equivalent to \code{slot(obj, "group") and returns the grouping variable.}
}
\usage{
AprioriGrouping(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
}
\details{
If a grouping vector is supplied function \code{VegsoupDataPartition} will automatically use if it is called without any arguments.
}
\value{
a vector of mode \code{numeric} assigning plots to groups coded with integers.
%% ...
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
dta <- VegsoupData(Vegsoup(species, sites, taxonomy, group = c(rep(1, 3), rep(2, 3)),
	scale = list(scale = "Braun-Blanquet")))
AprioriGrouping(dta)
}

\seealso{
\code{\link{Partitioning}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
