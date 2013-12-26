\name{Murdoch}
\docType{methods} 

\alias{murdoch}
\alias{murdoch-methods}
\alias{murdoch,VegsoupPartition-method}

\title{ Indicator Species Analysis by Murdoch Preference Function }

\description{
This is a wrapper for function
\code{\link[optpart:murdoch]{murdoch}} in package \pkg{optpart}.
}

\usage{
\S4method{murdoch}{VegsoupPartition}(x, minplt, type, ...)
}

\arguments{
  \item{ x }{
    \code{'VegsoupPartition'} object.
  }
  \item{ minplt, type }{
    see \\code{\link[optpart:murdoch]{murdoch}}
  }
  \item{ \dots }{
    not used.
  }
}

\value{
  a list object of class \code{"murdoch"}
}

\author{
  Implemented for \pkg{vegsoup} from function \code{murdoch} by \emph{David W.
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
