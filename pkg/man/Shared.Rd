\name{shared}
\docType{methods}

\alias{shared}
\alias{shared,VegsoupPartition-method}

\title{ Shared Species }

\description{
  Compute a diagonal matrix showing percentages of shared species between partitons.
}

\usage{
\S4method{shared}{VegsoupPartition}(x)
}

\arguments{
  \item{ x }{
     \code{VegsoupPartition} object.
  }
}

\details{
  The computed index is this equal to \code{designdist(t(Constancy(prt) > 0),
  method = "J/(A+B)*100")} and simply returns the percentage of shared species
  between partitions.
}

\value{
  an object of class \code{\link{dist}}.
}
\author{ Roland Kaiser }

\seealso{
\code{\link{vegdist}}
}
\examples{
library(vegsoup)

# real data
require(vegsoup)

data(barmstein)
x <- VegsoupPartition(barmstein, k = 2)

shared(x)
}
\keyword{ methods }

