\encoding{UTF-8}

\name{layer}
\docType{methods}

\alias{layer}
\alias{layer,Vegsoup-method}

\title{ Return Stratum/Layer }

\description{
Simply return the layer for each observation.
}

\usage{
\S4method{layer}{Vegsoup}(obj, ...)
}

\arguments{
  \item{ obj }{
    \code{Vegsoup*} object
  }
  \item{ \dots }{
    passed from to methods.
  }
}

\details{
  Simply returns the layer for each observation, equivalent to
  \code{species(obj)$layer}.
}

\value{
  \code{layer} returns a character vector equal in length to
  \code{nrow(slot(species(obj), "data"))}}

\author{ Roland Kaiser }

\note{
  The function will be renamed to avoid typos with method \code{layers} or
  even dropped -- it is not very useful and simple minded?
}

\seealso{
\code{\link{layers}}, \code{\linkS4class{Vegsoup}}
}

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

# check the number of occurences per layer
table(layer(x))

# alternative
table(species(x)$layer)
}
\keyword{ methods }

