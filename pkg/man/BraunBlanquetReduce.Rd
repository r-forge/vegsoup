\name{BraunBlanquetReduce}
\alias{BraunBlanquetReduce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reduce Braun-Blanquet abunace scale
}
\description{
Revert Braun-Blanquet abunace scale from extended to simple.
}
\usage{
BraunBlanquetReduce(obj, value)
}
\arguments{
  \item{obj}{an object inheriting from class \code{\link{Vegsoup-class}} }
}
\details{
Converts from 9 and 7 point Braun-Blanquet scale by merging abundance classes "2m", "2a" and "2b" to "1".
}
\value{
returns an object depending on the input class. 
}
\references{

}
\author{
Roland Kaiser
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{{AbundanceScale}}} and \code{\link{{AbundanceScale}<-}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
