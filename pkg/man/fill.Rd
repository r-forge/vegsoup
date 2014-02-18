\name{fill}
\docType{methods}

\alias{fill}
\alias{fill,Vegsoup-method}

\title{ Matrix fill of species matrix }

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
require(vegsoup)

data(barmstein)

fill(barmstein)
}
\keyword{ methods }

