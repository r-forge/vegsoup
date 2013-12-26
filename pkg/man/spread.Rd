\name{spread}
\docType{methods}

\alias{spread}
\alias{spread-methods}
\alias{spread,VegsoupPartition-method}

\title{ List Occurences of Species in Partitions }
\description{
  The method simply lists all occurences of species in partitions.
}
\usage{
  spread(obj)
}
\arguments{
  \item{ obj }{
    An object inheriting from \code{"VegsoupPartition"}.
  }
}
\value{
  A \code{"list"} with any many components as there are species-layer
  replicates (\code{ncol(obj)})
}
\author{ Roland Kaiser }
\seealso{
\code{\linkS4class{VegsoupPartition}}, ~~~
}
\examples{
require(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")
prt <- VegsoupPartition(dta, k = 2)

spread(prt)
}
\keyword{ methods }
