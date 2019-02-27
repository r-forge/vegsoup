\encoding{UTF-8}

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
  spread(obj, condensed = FALSE)
}
\arguments{
  \item{ obj }{
    An object inheriting from \code{"VegsoupPartition"}.
  }
  \item{ obj }{
    \code{logical}. If \code{TRUE} return in condensed form with as manny
    elements as there are partitions.
  }
}
\value{
  \code{"list"} with as many components as there are species-layer
  replicates (\code{ncol(obj)}).
}
\author{ Roland Kaiser }
\seealso{
\code{\linkS4class{VegsoupPartition}}
}
\examples{
require(vegsoup)

data(barmstein)
x <- VegsoupPartition(barmstein, k = 2)

spread(x)

spread(x, condensed = TRUE)
}
\keyword{ methods }

