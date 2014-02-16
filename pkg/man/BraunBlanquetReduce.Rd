\name{BraunBlanquetReduce}
\alias{BraunBlanquetReduce}
\alias{BraunBlanquetReduce,Vegsoup-method}

\title{ Reduce 9 Point Braun-Blanquet Abundance Scale to 7 Point }

\description{
Revert Braun-Blanquet abundance scale from extended (new) to simple (old).
}

\usage{
\S4method{BraunBlanquetReduce}{Vegsoup}(x)
}

\arguments{
  \item{ x }{
   \code{Vegsoup*} object.
  }
}

\details{
  Converts from 9 to 7 point Braun-Blanquet scale by merging abundance
  classes "2a" and "2b" to "2" and "2m" to "1". Of course, there is no way
  to convert from 7 to 9 point scale.
}

\value{
  returns an object depending on the input class. 
}

\references{
  Peet, R.K. & Roberts D.W. (2013). Classification of natural and semi-natural
  vegetation. In Vegetation Ecology (van der Maarel, E. & Franklin, J. eds).
  Wiley-Blackwell.
}

\author{ Roland Kaiser }

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

xr <- BraunBlanquetReduce(x)
coverscale(x)
coverscale(x)
class(BraunBlanquetReduce(x))

}
\seealso{
\code{\linkS4class{Coverscale}},
\code{\link{coverscale}}
}
\keyword{ methods }
