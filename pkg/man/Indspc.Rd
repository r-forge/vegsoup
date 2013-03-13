\encoding{UTF-8}
\name{Indspc}
\docType{methods}
\alias{Indspc}
\alias{Indspc-methods}

\title{ Compositional Indicator Species Analysis }
\description{
Calculates the mean similarity of all plots in which a species occurs.
}
\usage{
## S4 method for signature 'Vegsoup*'
Indspc(obj, ...)
}
\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ \dots }{
    arguments passed to \code{labdsv::indspc} to specify \code{numitr}.
  }
}
\details{
  See \code{\link[labdsv:indspc]{indspc}} for details.
}
\value{
  An object of class \code{"indspc"}. See \code{\link[labdsv:indspc]{indspc}}
  in package \pkg{labdsv}.
}
\author{
Interfaced from Roland Kaiser for package vegsoup from function
\code{indspc} by \emph{David. W. Roberts} in package \pkg{labdsv}.
}
\seealso{
\code{\link{Fidelity}}
}
\examples{
require(vegsoup)

data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

Indspc(dta)
}
\keyword{ methods }
