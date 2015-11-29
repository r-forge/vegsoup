\encoding{UTF-8}

\name{as.data.frame}
\docType{methods}

\alias{as.data.frame}
\alias{as.data.frame,Vegsoup-method}
\alias{as.data.frame,Sites-method}

\title{ Coerce to data.frame }

\description{
  Besides the \code{as.matrix} and alike methods that return just the
  species matrix of \code{Vegsoup*} objects, an essential requirement is the
  coercion to class \code{"data.frame"} for which the plethora of \R functions for
  single-table data becomes available to \code{Vegsoup*} objects. Because this
  coercion is fundamental, the syntax to execute is as simple as possible.
  
  The \code{as.data.frame} method for objects of class \code{Sites}
  provides reshaping the data to wide format (cp. \code{\link{stackSites}}).
  
}

\usage{
\S4method{as.data.frame}{Vegsoup}(x)

\S4method{as.data.frame}{Sites}(x)
}

\arguments{
  \item{ x }{
	\code{Vegsoup*} object.
  }
}

\details{
  For Vegsoup objects, the \code{as.data.frame} method is essentially a database
  join operation. Unlike the data stored in a \code{Vegsoup*} object, the returned data frame
  has redundancy (e.g. as many rows by plot as there are species).
}

\author{ Roland Kaiser }

\seealso{
\code{\link{as.matrix}}
}

\examples{
require(vegsoup)
data(barmstein)

#	for Vegsoup objects
class(as(barmstein, "data.frame"))
class(as.data.frame(barmstein))

#	automatic dispatch
summary(lm(hcov ~ scov, data = layers(barmstein, collapse = "E")))

#	for Sites obejcts
#	create a dummy object first
y <- data.frame(
		plot = rep(1:3, each = 3),
		variable = rep(rep(paste0("var", 1:3)), times = 3),
		value = rnorm(3 * 3))

y <- sites(y)

as.data.frame(y)
}

\keyword{ methods }

