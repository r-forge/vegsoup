\name{spread}
\docType{methods}

\alias{spread}
\alias{spread-methods}
\alias{spread,VegsoupPartition-method}

\title{ List Occurences of Species in Partitions }
\description{
  The method simply lists all occurrences of species in partitions.
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
  \code{"list"} with as many components as there are species-layer
  replicates (\code{ncol(obj)}).
}
\author{ Roland Kaiser }
\seealso{
\code{\linkS4class{VegsoupPartition}}, ~~~
}
\examples{
require(vegsoup)

data(barmstein)
x <- VegsoupPartition(barmstein, k = 2)

spread(x)
}
\keyword{ methods }

