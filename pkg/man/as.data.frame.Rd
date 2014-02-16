\name{as.data.frame}
\docType{methods}

\alias{as.data.frame}
\alias{as.data.frame,Vegsoup-method}

\title{ Coerce to data.frame }

\description{
  Besides the \code{as.matrix} and alike methods that return just the
  species matrix of \code{Vegsoup*} objects, an essential requirement is the
  coercion to class \code{"data.frame"} for which the plethora of R functions for
  single-table data, become available to \code{Vegsoup*} objects. Because this
  coercion is fundamental, the syntax to execute it is also is as simple
  as possible.
}

\usage{
\S4method{as.data.frame}{Vegsoup}(x)
}

\arguments{
  \item{ x }{
    \code{Vegsoup*} object.
  }
}

\details{
  The \code{"as.data.frame"} method is essentially a database join operation.
  Unlike the data stored in a \code{Vegsoup*} object, the returned data frame
  has redundancy (e.g. as many rows by plot as there are species).
}

\author{ Roland Kaiser }

\seealso{
\code{link{as.matrix}}
}

\examples{
require(vegsoup)
data(barmstein)

class(as(barmstein, "data.frame"))
class(as.data.frame(barmstein))

summary(lm(hcov ~ scov, data = Layers(barmstein, collapse = "E")))

}

\keyword{ methods }