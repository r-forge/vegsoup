\encoding{UTF-8}

\name{isamic}
\docType{methods}

\alias{isamic}
\alias{isamic-methods}
\alias{isamic,VegsoupPartition-method}

\title{ Indicator Species Analysis to Minimize Intermediate Constancy }

\description{
  This is a wrapper for function
  \code{\link[labdsv:isamic]{isamic}} in package \pkg{labdsv}.
}

\usage{
  \S4method{isamic}{VegsoupPartition}(x)
}

\arguments{
  \item{x}{
	\code{'VegsoupPartition'} object.
  }
}
\value{
a vector of indictor values.
}
\author{
  Implemented for vegsoup from function \code{isamic} by \emph{David W. Roberts}
  in package \pkg{labdsv}.
}
\seealso{
\code{\linkS4class{VegsoupPartition}},
\code{\link{fidelity}}
}
\examples{
require(vegsoup)

data(barmstein)

isamic(VegsoupPartition(barmstein, k = 2))
}
\keyword{ methods }

