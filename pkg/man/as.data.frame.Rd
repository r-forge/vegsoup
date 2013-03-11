\name{as.data.frame}
\docType{methods}
\alias{as.data.frame}
\alias{as.data.frame-method}

\title{ Retrieve Sites Dataframe }
\description{
  Besides the \code{as.matrix} and alike methods that return just the
  species matrix of \code{Vegsoup*} objects, an esstential requirement is the
  coercion to class \code{"data.frame"} for which the variety of R tools for
  single-table data, become available to code{Vegsoup*} objects. Because this
  coercion is fundamental, the syntax to execute it is also is as simple
  as possible.
}
\usage{
as.data.frame(obj)
}
\arguments{
  \item{ x }{
    A \code{Vegsoup*} object.
  }
}
\details{
  The \code{"as.data.frame"} method is essentially a database join operation.
  Unlike the data stored in a \code{Vegsoup*} object, the returned data frame
  has redundancy.
}
\author{ Roland Kaiser }

\seealso{
\code{link{as.matrix}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

class(as(dta, "data.frame"))
class(as.data.frame(dta))

summary(lm(hcov ~ scov, data = dta))

}

\keyword{ methods }