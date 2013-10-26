\name{BraunBlanquetReduce}
\alias{BraunBlanquetReduce}

\title{ Reduce 9 Point Braun-Blanquet Abundance Scale to 7 Point }

\description{
Revert Braun-Blanquet abundance scale from extended (new) to simple (old).
}

\usage{
BraunBlanquetReduce(x)
}

\arguments{
  \item{ x }{
    An \code{Vegsoup*} object.
  }
}

\details{
  Converts from 9 and 7 point Braun-Blanquet scale by merging abundance
  classes "2m", "2a" and "2b" to "2".
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
library(vegsoup)

data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

red <- BraunBlanquetReduce(dta)
coverscale(dta)
coverscale(red)
class(BraunBlanquetReduce(red))

}
\seealso{
\code{\linkS4class{Coverscale}},
\code{\link{coverscale}}
}
\keyword{ methods }
