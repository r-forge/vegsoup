\name{Shared}
\alias{Shared}

\title{
Compare Shared Species between Partitions
}
\description{
Compute a diagonal matrix showing percentages of shared species between partitons.
}
\usage{
Shared(obj)
}
\arguments{
  \item{ obj }{
     an object inheriting from class \code{"VegsoupPartition"}.
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
