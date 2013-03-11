\name{as.data.list}
\docType{methods}
\alias{as.data.list}
\alias{as.data.list-method}

\title{ Coercion to \code{"data.lists"} }
\description{
  This function coerces \code{Vegsoup*} objects to data lists defined in
  package multitable. See \code{\link[multitable:as.data.list]{as.data.list}}
  for details.
}
\usage{
as.data.list(obj)
}
\arguments{
  \item{ x }{
    A \code{Vegsoup*} object.
  }
}
\details{
  This a a convinience function for coercion to \code{"data.list"} objects.
  The current interface is rudimentary and does not accept additional
  arguments supplied by function \code{\link{dlcast}} which is used to make
  the conversion.
}
\author{ Roland Kaiser }

\seealso{
\code{\link{as.data.frame}}
\code{\link{as.matrix}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

x <- as(dta, "data.list")
x <- as.data.list(dta)
summary(x)
}
\keyword{ methods }