\name{as.data.frame}
\docType{methods}
\alias{as.data.frame}
\alias{as.data.frame-method}

\title{ Retrieve Sites Dataframe }
\description{
  In contrast to the \code{as.matrix} and alike methods that return the
  species matrix of \code{Vegsoup*} objects, the \code{as.data.frame} dispatch
  is rarely needed, as it adds no significant functionality compared to the
  slot acessor method \code{\link{Sites}}. Both return equal objects.
}
\usage{
as.data.frame(obj)
}
\arguments{
  \item{ x }{
    A \code{Vegsoup*} object.
  }
}

\author{ Roland Kaiser }

\seealso{
\code{link{as.matrix}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")
as.data.frame(dta)
}
\keyword{ methods }