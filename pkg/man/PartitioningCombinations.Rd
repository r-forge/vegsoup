\name{PartitioningCombinations}
\alias{PartitioningCombinations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
All possible combinations of partitions
}
\description{
Tabulate all possibele combination of partitions  
}
\usage{
PartitioningCombinations(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
    an object inhertiting from class \code{"VegsoupDataPartition"}
}
  \item{collapse}{
    character of be be used as seperator in dimnames.
  }
}
\details{
Taken from function \code{\link{multipatt}} in package indicspecies.
}
\value{
returns a \code{"matrix"} where combinations are codes in \code{dimnames()[[2]] }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Miquel De Cáceres Ainsa & Florian Jansen
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
