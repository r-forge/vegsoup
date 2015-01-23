\encoding{UTF-8}

\name{sites Vegsoup}
\docType{methods}

\alias{sites}
\alias{sites,Vegsoup-method}

\alias{sites<-}
\alias{sites<-,Vegsoup-method}
\alias{sites<-,Vegsoup,data.frame-method}

\title{ Methods for the Sites Slot of \code{Vegsoup*} Objects }

\description{
Accessor and replacement methods acting on the \code{'sites'} slot.
}
\usage{
\S4method{sites}{Vegsoup}(obj)
\S4method{sites}{Vegsoup}(obj) <- value
}

\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ value }{
    An object of class \code{"data.frame"}.
  }

}
\details{
  The value object in the replacement function needs to have rownames matching
  \code{rownames(obj)}. The order is respected and leads to reordering of the
  object to which the value object is assigned.
}
\value{
  For \code{sites(obj)} a data.frame,
  and for the replacement method an object depending on the input class.
}
\author{ Roland Kaiser }
\seealso{
\code{\linkS4class{Vegsoup}}
}
\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

# use replacemnet to shuffe order
rownames(x)
sites(x) <- sites(x)[sample(rownames(x)), ]
rownames(x)

}
\keyword{ methods }

