\encoding{UTF-8}

\name{AprioriGrouping}
\docType{methods}

\alias{AprioriGrouping}
\alias{AprioriGrouping,Vegsoup-method}

\title{ Grouping Variable of \code{Vegsoup*} Objects }
\description{
Returns the grouping variable. Equivalent to \code{slot(obj, "group")}.
}

\usage{
\S4method{AprioriGrouping}{Vegsoup}(obj)
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
require(vegsoup)
data(barmstein)

AprioriGrouping(barmstein)
}
\keyword{ methods }

