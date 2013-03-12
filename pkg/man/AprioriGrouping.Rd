\name{AprioriGrouping}
\docType{methods}
\alias{AprioriGrouping}
\alias{AprioriGrouping-methods}

\title{ Grouping Variable of \code{Vegsoup*} Objects }
\description{
Returns the grouping variable. Equivalent to \code{slot(obj, "group")}.
}
\usage{
## S4 method for signature 'Vegsoup*'
AprioriGrouping(obj)
}

\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
}
\details{
  If a grouping vector is supplied function \code{VegsoupPartition} will
  automatically use if it is called without any arguments.
}
\value{
  a vector of mode \code{numeric} assigning plots to groups coded as integers.
}
\author{ Roland Kaiser }

\seealso{
\code{\link{Partitioning}}
}

\examples{
library(vegsoup)
data(testdata)

dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

AprioriGrouping(dta)
}
\keyword{ methods }
