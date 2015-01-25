\encoding{UTF-8}

\name{nndist}
\docType{methods}

\alias{nndist}
\alias{nndist,Vegsoup-method}

\title{ Nearest Neighbor Distances }

\description{
  Identifies the nearest neighbor of each plot based on dissimilarity matrix.
}

\usage{
\S4method{nndist}{Vegsoup}(X, ...)
}

\arguments{
  \item{ X }{
	A \code{Vegsoup*} object.
  }
  \item{ \dots }{
	Not used.
  }
}

\details{
Nothing here yet!
}

\value{
  Returns a numeric vector of distances equal to the number of plots. The identified nearest
  neighboring plot is given as attribute. See \sQuote{Examples}.
}
\author{ Roland Kaiser }
%\references{
%}
\seealso{
\code{\link{outlier}}, \code{\link{as.dist}}
}
\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

vegdist(x) <- "bray"
( r <- nndist(x) )

# get nearest neighbour plot names
attr(r, "neighbour")

}
\keyword{methods}

