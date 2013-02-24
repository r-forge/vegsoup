\name{AprioriGrouping}
\docType{methods}
\alias{AprioriGrouping}
\alias{AprioriGrouping-methods}
\alias{AprioriGrouping,Vegsoup-method}

\title{
Get Grouping Variable of Object Extending Class Vegsoup
}
\description{
Equivalent to \code{slot(obj, "group") and returns the grouping variable.}
}
\usage{
## S4 method for signature 'Vegsoup*'
AprioriGrouping(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
An object of class \code{Vegsoup*}.
}
}
\details{
If a grouping vector is supplied function \code{VegsoupPartition} will automatically use if it is called without any arguments.
}
\value{
a vector of mode \code{numeric} assigning plots to groups coded as integers.
}
\author{
Roland Kaiser
}

\examples{
library(vegsoup)
data(testdata)

dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

AprioriGrouping(dta)
}

\seealso{
\code{\link{Partitioning}}
}

\keyword{ methods }

