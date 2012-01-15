\name{StablePartition}
\alias{StablePartition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjusted Goodman-Kruskal's lambda index of classification stability
}
\description{
Adjusted Goodman-Kruskal's lambda index of classification stability calculated from crosstabulation of clusters obtained from the data set and the subset selected by without replacement bootstrap.
}
\usage{
StablePartition(obj, nitr = 200, nitr.lambda = 10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{an object of class \code{\link{VegsoupDataPartition}}}
  \item{intro}{number of iterations}
  \item{nitr.lambda}{number of iteration to calculate random values of lambda}
  \item{\dots}{arguments passed to VegsoupDataPartition}
}
\details{
Adjusted Goodman-Kruskal's lambda index of classification stability calculated from crosstabulation of clusters obtained from the data set and the subset selected by without replacement bootstrap
}
\value{
returns a list with components
\item{lambda}{lambda statistic}
\item{modified.lambda}{modified lambda statistic}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
original authors, Roland Kaiser
}
\note{
adopted for vegsoup from script Function Classification Stability supplied for JUICE software (\url{http://www.sci.muni.cz/botany/juice/})
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
