\name{Isamic}
\docType{methods}

\alias{Isamic}
\alias{Isamic-methods}
\alias{Isamic,VegsoupPartition-method}

\title{ Indicator Species Analysis to Minimize Intermediate Constancy }

\description{
  This is a wrapper for function
  \code{\link[optpart:isamic]{isamic}} in package \pkg{optpart}.
}

\usage{
  \S4method{Isamic}{VegsoupPartition}(obj)
}

\arguments{
  \item{obj}{
    A \code{VegsoupPartition} object.
  }
}
\value{
a vector of indictor values.
}
\author{
  Implemented for vegsoup from function \code{isamic} by \emph{David W.
Roberts} in package \pkg{optpart}.
}
\seealso{
\code{\linkS4class{VegsoupPartition}},
\code{\link{Fidelity}}
}
\examples{
require(vegsoup)

data(testdata)
dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")
prt <- VegsoupPartition(dta, k = 2)

Isamic(prt)
}
\keyword{ methods }