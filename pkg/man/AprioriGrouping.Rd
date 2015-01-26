\encoding{UTF-8}

\name{apriori}
\docType{methods}

\alias{apriori}
\alias{apriori,Vegsoup-method}

\title{ Grouping Variable of \code{Vegsoup*} Objects }
\description{
Returns the grouping variable. Equivalent to \emph{slot group)}.
}

\usage{
\S4method{apriori}{Vegsoup}(obj)
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
\code{\link{partitioning}}
}

\examples{
require(vegsoup)
data(barmstein)

apriori(barmstein)
}
\keyword{ methods }

