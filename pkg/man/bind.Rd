\name{bind}

\alias{bind}
\alias{bind,Vegsoup-method}

\title{ Combine \code{Vegsoup*} objects }
\description{
Take a sequence of arguments of objects from class \code{Vegsoup*} and combine them into a single object.
}

\usage{
  \S4method{bind}{Vegsoup}(..., deparse.level = 1)
}

\arguments{
  \item{ \dots }{
    \code{Vegsoup} objects.
  }
  \item{ deparse.level }{
    Not used.
  }
}

\details{
By calling \code{bind} all slots of the input objects are combined and a new object is returned. Note, if a list of \code{Vegsoup*} objects needs to be combined, simply call \code{do.call("bind", list.of.Vegsoup.objects)} (see \sQuote{Examples}).
}
\value{
On object of class \code{VegsoupData}.
}

\section{Warning}{
The method raises an error if plot names are not unique after combining objects.
}
\author{
Roland Kaiser
}
\seealso{
\code{\linkS4class{Vegsoup}}
}
\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

# split object in three parts
s1 <- x[1:2, ]
s2 <- x[3:4, ]
s3 <- x[5:6, ]

# combine them
xb <- bind(s1, s2, s3)

# equivalent
ss <- list(s3, s1, s2)
xb <- do.call("bind", ss)

# note, ordering is changed
( r0 <- rownames(x) )
( rb <- rownames(xb) )
# restore the orignal order
rownames(xb[match(r0, rb), ])


}
\keyword{methods}

