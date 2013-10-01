\name{fill}
\docType{methods}
\alias{fill}
\alias{fill-methods}

\title{ Matrix fill of ASpecies Matrix for \code{Vegsoup*} Objects }

\description{
  Calculates the the proportion (percentage) of all non zero elements in a
  species matrix.
}
\usage{
  fill(obj)
}
\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
}
\value{
  returns a vector of length one and mode \code{"numeric"} 
}
\author{ Roland Kaiser }
\seealso{
  \code{\link{nestedtemp}} in \pkg{vegan} for nestedess indices.
}
\examples{
library(vegsoup)

data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")
fill(dta)
}
\keyword{ methods }
