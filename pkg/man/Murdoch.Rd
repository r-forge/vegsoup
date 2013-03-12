\name{Murdoch}
\docType{methods} 
\alias{Murdoch}
\alias{Murdoch-methods}
\alias{Murdoch,VegsoupPartition-method}

\title{ Indicator Species Analysis by Murdoch Preference Function }

\description{
This is a wrapper for function
\code{\link[optpart:murdoch]{murdoch}} in package \pkg{optpart}.
}
\usage{
Murdoch(obj, ...)
}
\arguments{
  \item{obj}{
    A \code{VegsoupPartition} object.
  }
  \item{\dots}{
    Arguments passed to \code{optpart::murdoch}
  }
}
\value{
  a list object of class \code{"murdoch"}
}
\author{
  Implemented for vegsoup from function \code{murdoch} by \emph{David W.
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
Murdoch(prt)
}
\keyword{ methods }
