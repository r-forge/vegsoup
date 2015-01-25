\encoding{UTF-8}

\name{as.data.list}
\docType{methods}

\alias{as.data.list}
\alias{as.data.list,Vegsoup-method}

\title{ Coercion to \code{"data.lists"} }

\description{
  This function coerces \code{Vegsoup*} objects to data lists defined in
  package multitable. See \code{\link[multitable:as.data.list]{as.data.list}}
  for details.
}

\usage{
\S4method{as.data.list}{Vegsoup}(x)
}

\arguments{
  \item{ x }{
	A \code{Vegsoup*} object.
  }
}

\details{
  This a convenience coercion method for translation to \code{"data.list"} objects.
  The current interface is rudimentary and does not accept additional
  arguments supplied by function \code{\link{dlcast}}, although it is used internaly
  to perform the conversion.
}

\author{ Roland Kaiser }

\seealso{
\code{\link{as.data.frame}}, \code{\link{as.matrix}}
}

\examples{
require(vegsoup)
data(barmstein)

x <- as(barmstein, "data.list")
summary(x)

\dontrun{
# not implemented yet
x <- as.data.list(barmstein)
}

}

\keyword{ methods }

