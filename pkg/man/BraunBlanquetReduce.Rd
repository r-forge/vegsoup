\name{BraunBlanquetReduce}
\alias{BraunBlanquetReduce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reduce 9 Point Braun-Blanquet Abundance Scale to 7 Point 
}
\description{
Revert Braun-Blanquet abundance scale from extended to simple.
}
\usage{
BraunBlanquetReduce(obj)
}
\arguments{
  \item{obj}{an object inheriting from class \code{\link{Vegsoup}} }
}
\details{
Converts from 9 and 7 point Braun-Blanquet scale by merging abundance classes "2m", "2a" and "2b" to "2".
}
\value{
returns an object depending on the input class. 
}
\references{

}
\author{
Roland Kaiser
}
\examples{
library(vegsoup)

data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

red <- BraunBlanquetReduce(dta)
AbundanceScale(dta)
AbundanceScale(red)
class(BraunBlanquetReduce(red))

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{AbundanceScale}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
