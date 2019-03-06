\encoding{UTF-8}

\name{spread}
\docType{methods}

\alias{spread}
\alias{spread-methods}
\alias{spread,VegsoupPartition-method}
\alias{private}
\alias{private-methods}
\alias{private,VegsoupPartition-method}

\title{ Occurrences of Species in Partitions }
\description{
  Method \code{spread} simply lists all occurrences of species labeled by
  partition. \code{private} returns the \emph{private} species, those occurring
  in a specific partition only.
}
\usage{
  spread(obj, condensed = FALSE)
  
  private(obj)
}
\arguments{
  \item{ obj }{
    An object inheriting from \code{"VegsoupPartition"}.
  }
  \item{ condensed }{
    \code{logical}. If \code{TRUE} return in condensed form with as manny
    elements as there are partitions.
  }
}
\value{
  For \code{spread} a \code{"list"} with as many components as there are
  species-layer replicates (\code{ncol(obj)}).

  For \code{private} a logical matrix with as many rows as there are
  species-layer replicates and columns according to the number of partitions
  (\verb{getK(obj)}).
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

private(x)
}
\keyword{ methods }

