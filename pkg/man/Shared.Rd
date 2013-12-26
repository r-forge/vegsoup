\name{shared}
\docType{methods}

\alias{shared}
\alias{shared,VegsoupPartition-method}

\title{ Compare shared species between partitions }

\description{
  Compute a diagonal matrix showing percentages of shared species between partitons.
}

\usage{
\S4method{shared}{VegsoupPartition}(x)
}

\arguments{
  \item{ x }{
     An \code{VegsoupPartition} object.
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
data(bigtestdata)

dta <- Vegsoup(spc.big, sts.big, txa.big, "braun.blanquet")
prt <- VegsoupPartition(dta, k = 3)

Shared(prt)
}
\keyword{ methods }
